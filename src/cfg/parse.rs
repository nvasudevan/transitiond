use std::{fs, io};
use regex::Regex;
use lazy_static::lazy_static;
use crate::cfg::{TermSymbol, LexSymbol, NonTermSymbol, EpsilonSymbol};

#[derive(Debug)]
pub(crate) struct RultAltStr {
    alt: Vec<LexSymbol>
}

impl RultAltStr {
    pub(crate) fn new(alt: Vec<LexSymbol>) -> Self {
        Self {
            alt
        }
    }
}

#[derive(Debug)]
pub(crate) struct CfgRuleStr {
    pub(crate) lhs: String,
    pub(crate) rhs: Vec<RultAltStr>
}

impl CfgRuleStr {
    pub(crate) fn new(lhs: String, rhs: Vec<RultAltStr>) -> Self {
       Self {
           lhs,
           rhs
       }
    }
}

pub(crate) struct CfgParser {
    start_symbol: String,
    rules: Vec<CfgRuleStr>,
}

impl CfgParser {
    fn new() -> Self {
        Self {
            start_symbol: String::new(),
            rules: vec![],
        }
    }

    fn add_start_symbol(&mut self, start_symbol: String) {
        self.start_symbol = start_symbol;
    }

    fn add_rule(&mut self, rule: CfgRuleStr) {
        self.rules.push(rule);
    }
}

/// skip the whitespace (' ', \n) and move the counter to the next non-ws.
fn ws(s_chars: &[char], i: usize) -> Option<usize> {
    let mut j = i;
    while j < s_chars.len() {
        let c = s_chars.get(j)?;
        if ! c.is_ascii_whitespace() {
            // we have a non-ws character
            return Some(j);
        }
        j += 1;
    }

    Some(j)
}

/// Retrieve the next token. Read until the next character is not ASCII.
fn next_token(s_chars: &[char], i: usize) -> Option<(String, usize)> {
    let mut j = ws(s_chars, i)?; // j points to the non-ws
    let mut z = j;
    while z < s_chars.len() {
        let c = s_chars.get(z)?;
        if !c.is_ascii_alphabetic() {
            break;
        }
        z += 1;
    }

    let tok_chars = s_chars.get(j..z + 1)?;
    let tok: String = tok_chars.iter().collect();

    Some((tok, z))
}

/// skip past %define directive, read until we encounter the next % sign
fn parse_define_directive(s_chars: &[char], i: usize) -> Option<usize> {
    let peep = s_chars.get(i..i + 7)?;
    let peep_s: String = peep.iter().collect();
    if peep_s.eq("%define") {
        let mut j = i + 7;
        while j < s_chars.len() {
            let c = s_chars.get(j)?;
            if *c == '%' {
                return Some(j);
            }
            j += 1;
        }
    }

    Some(i)
}

fn parse_start_directive(s_chars: &[char], i: usize, cfg_parser: &mut CfgParser) -> Option<usize> {
    let mut j = i;
    // %start - look ahead next 5 symbols to check
    let peep = s_chars.get(j..j + 6)?;
    let peep_s: String = peep.iter().collect();
    if peep_s.eq("%start") {
        j += 6; //%start. root
        // start at . and read the next token, move the reference to after token
        let (tok, k) = next_token(s_chars, j)?;
        j = k;
        cfg_parser.add_start_symbol(tok);
    }

    Some(j)
}

/// check for `%%` directive; if so, move the pointer
fn rules_marker_directive(s_chars: &[char], i: usize) -> Option<usize> {
    let mut j = ws(s_chars, i)?; // j points to the non-ws
    let peep = s_chars.get(j..j + 2)?;
    let s: String = peep.iter().collect();
    if s.eq("%%") {
        j += 2;
    }

    Some(j)
}

/// Parse bison/YACC directives:
/// - %define - marks the definition part
/// - %start - marks the start rule part
/// - %% - has two of these, marks the begin and end of rules section.
fn parse_header_directives(s_chars: &[char], i: usize, cfg_parser: &mut CfgParser) -> Option<usize> {
    let mut j = i;
    if let Some(z) = parse_define_directive(s_chars, i) {
        j = z;
        if j > i {
            println!("%define, i:{} -> j:{}", i, j);
            j = parse_start_directive(s_chars, j, cfg_parser)
                .expect("Unable to parse %start directive!");
            println!("%start, j:{}", j);
            j = rules_marker_directive(s_chars, j)
                .expect("Unable to parse %% directive at the end of header!");
            println!("%% matched, j:{}", j);
            return Some(j);
        }
    }

    Some(i)
}

/// Parses the footer `%%` section, thus marking the end of parsing
fn parse_footer_directive(s_chars: &[char], i: usize) -> Option<usize> {
    let j = rules_marker_directive(s_chars, i)?;
    if j > i {
        println!("[END]");
        return Some(j);
    }

    Some(i)
}

/// read until `;`, otherwise we have reached end of rule section
fn rule_end_marker(s_chars: &[char], i: usize) -> Option<usize> {
    let mut j = i;
    while j < s_chars.len() {
        let c = s_chars.get(j)?;
        if *c == ';' {
            return Some(j);
        }
        j += 1;
    }

    Some(i)
}

fn rule_regex(s: &str) -> Option<(&str, &str)> {
    // let re = Regex::new(r"(?m)(^[a-zA-Z]+):([a-zA-Z'|\s]+)(;$)")
    //     .expect("Unable to create regex");(?P<end>[\n\r\s]*;[\n\r\s]*)
    lazy_static! {
     static ref RE_RULE: Regex = Regex::new(r"(?P<lhs>[a-zA-Z]+)[\s]*:(?P<rhs>[a-zA-z'|\s]+)[\s]*;")
        .expect("Unable to create regex");
    }
    let cap = RE_RULE.captures(s)?;
    // println!("cap: {:?}", cap);
    let lhs = cap.name("lhs")?.as_str();
    let rhs = cap.name("rhs")?.as_str();

    Some((lhs, rhs))
}

/// S: A 'b' C | 'x' | ;
fn rule_rhs_regex(s: &str) -> Option<Vec<RultAltStr>> {
    lazy_static! {
     static ref RE_ALT: Regex = Regex::new(r"(?P<alt>[a-zA-z'\s]+)")
        .expect("Unable to create regex for parsing rule alternatives");
     static ref RE_EMPTY_ALT: Regex = Regex::new(r"(?P<empty>\s*)")
        .expect("Unable to create regex to parse an empty alternative");
    }

    let alts: Vec<&str> = s.split('|').collect();
    let mut alts_s: Vec<RultAltStr> = vec![];
    for alt in alts {
        let alt_trimmed = alt.trim();
        match RE_ALT.captures(alt_trimmed) {
            Some(cap) => {
                let alt_s = cap.name("alt")?.as_str();
                let alts_syms = parse_alt(alt_s)?;
                alts_s.push(RultAltStr::new(alts_syms));
            },
            _ => {
                // try empty alt
                let empty_alt_cap = RE_EMPTY_ALT.captures(alt)?;
                let alt_s = empty_alt_cap.name("empty")?.as_str();
                let alt_syms: Vec<LexSymbol> = vec![LexSymbol::Epsilon(EpsilonSymbol::new("".to_owned()))];
                alts_s.push(RultAltStr::new(alt_syms));
            }
        }
    }

    Some(alts_s)
}

fn term_token_regex(s: &str) -> Option<TermSymbol> {
    lazy_static! {
     static ref RE_TERMINAL: Regex = Regex::new(r"(?P<tok>'[a-zA-z]+')")
        .expect("Unable to create regex for parsing a terminal token");
    }

    let cap = RE_TERMINAL.captures(s)?;
    let tok = cap.name("tok")?.as_str();

    Some(TermSymbol::new(tok.to_owned()))
}

fn non_term_token_regex(s: &str) -> Option<NonTermSymbol> {
    lazy_static! {
     static ref RE_NON_TERMINAL: Regex = Regex::new(r"(?P<tok>[a-zA-z]+)")
        .expect("Unable to create regex to parse a non-terminal token");
    }

    let cap = RE_NON_TERMINAL.captures(s)?;
    let tok = cap.name("tok")?.as_str();

    Some(NonTermSymbol::new(tok.to_owned()))
}

fn parse_lex_symbols(s: &str) -> Option<LexSymbol> {
    if let Some(sym) = term_token_regex(s) {
        return Some(LexSymbol::Term(sym));
    }

    if let Some(sym) = non_term_token_regex(s) {
        return Some(LexSymbol::NonTerm(sym));
    }

   None
}

fn parse_alt(s: &str) -> Option<Vec<LexSymbol>> {
    let tokens_s: Vec<&str> = s.split_ascii_whitespace().collect();
    let mut syms: Vec<LexSymbol> = vec![];
    for tok in tokens_s {
        let sym = parse_lex_symbols(tok)?;
        syms.push(sym);
    }

    Some(syms)
}

/// Create a rule
/// From `i`, tries the rule end marker `;`, if found, returns the rule.
/// Otherwise, returns `i`.
fn rule(s_chars: &[char], i: usize, cfg_parser: &mut CfgParser) -> Option<usize> {
    let mut j = rule_end_marker(s_chars, i)?;
    if j > i {
        let rule = s_chars.get(i..j+1)?;
        let rule_s: String = rule.iter().collect();
        let (lhs, rhs) = rule_regex(&rule_s)
            .expect("Unable to parse lhs, rhs of rule");
        let alts = rule_rhs_regex(rhs).unwrap();
        let cfg_rule = CfgRuleStr::new(lhs.to_owned(), alts);
        cfg_parser.rules.push(cfg_rule);
        return Some(j);
    }

    Some(i)
}

fn root_rule(s_chars: &[char], i: usize, mut cfg_parser: &mut CfgParser) -> Option<usize> {
    rule(s_chars, i, &mut cfg_parser)
}

/// Parses the rules section between the `%%` markers
/// First read the root rule and then parse the rest of the rules
fn parse_rules(s_chars: &[char], i: usize, mut cfg_parser: &mut CfgParser) -> Option<usize> {
    println!("=> parsing rules");
    let mut j = ws(s_chars, i)?;
    // read root rule
    j = root_rule(s_chars, j, &mut cfg_parser)?;
    j += 1;
    while j < s_chars.len() {
        j = rule(s_chars, j, &mut cfg_parser)?;
        j += 1;
        let c = s_chars.get(j)?;
        // at the end of rule section
        if *c == '%' {
           break
        }
    }

    Some(j)
}

fn read_file(cfgp: &str) -> io::Result<CfgParser> {
    let s = fs::read_to_string(cfgp)?;
    let mut s_chars: Vec<char> = s.chars().into_iter().collect();
    let mut cfg_parser = CfgParser::new();
    let mut i = ws(s_chars.as_slice(), 0)
        .expect("Unable to get first non-ws");
    i = parse_header_directives(s_chars.as_slice(), i, &mut cfg_parser)
        .expect("Unable to parse the header directives!");
    i = parse_rules(s_chars.as_slice(), i, &mut cfg_parser)
        .expect("Parsing of grammar rules failed!");
    parse_footer_directive(s_chars.as_slice(), i)
        .expect("Unable to parse footer directive");

    Ok(cfg_parser)
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    #[test]
    fn test_cfg() {
        let cfg_parser = read_file("./grammars/test.y").expect("xxx");
        println!("rules: \n{:?}", cfg_parser.rules);
    }

    #[test]
    fn test_regex() {
        let s = "root: BGH_C | ;";
        let re = Regex::new(r"(?P<lhs>[a-zA-Z]+):(?P<rhs>[a-zA-z'|\s]+)[\s]*;")
            .expect("Unable to create regex");
        let cap = re.captures(s)
            .expect("Unable to create capture");
        println!("cap: {:?}", cap);
    }
}
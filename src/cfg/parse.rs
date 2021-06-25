use std::fs;

use lazy_static::lazy_static;
use regex::Regex;

use crate::cfg::{CfgRule, EpsilonSymbol, LexSymbol, NonTermSymbol, RuleAlt, TermSymbol, Cfg};

pub(crate) struct CfgParser {
    tokens: Vec<char>,
    start_symbol: String,
}

impl CfgParser {
    fn new(tokens: Vec<char>) -> Self {
        Self {
            tokens,
            start_symbol: String::new(),
        }
    }

    fn add_start_symbol(&mut self, start_symbol: String) {
        self.start_symbol = start_symbol;
    }

    /// skip the whitespace (' ', \n) and move the counter to the next non-ws.
    fn ws(&self, i: usize) -> Option<usize> {
        let s_chars = self.tokens.as_slice();
        let mut j = i;
        while j < s_chars.len() {
            let c = s_chars.get(j)?;
            if !c.is_ascii_whitespace() {
                // we have a non-ws character
                return Some(j);
            }
            j += 1;
        }

        Some(j)
    }

    // TOKENS

    fn term_token_regex(&self, s: &str) -> Option<TermSymbol> {
        lazy_static! {
            static ref RE_TERMINAL: Regex = Regex::new(r"'(?P<tok>[a-zA-z]+)'")
            .expect("Unable to create regex for parsing a terminal token");
    }

        let cap = RE_TERMINAL.captures(s)?;
        let tok = cap.name("tok")?.as_str();

        Some(TermSymbol::new(tok.to_owned()))
    }

    fn non_term_token_regex(&self, s: &str) -> Option<NonTermSymbol> {
        lazy_static! {
            static ref RE_NON_TERMINAL: Regex = Regex::new(r"(?P<tok>[a-zA-z]+)")
            .expect("Unable to create regex to parse a non-terminal token");
    }

        let cap = RE_NON_TERMINAL.captures(s)?;
        let tok = cap.name("tok")?.as_str();

        Some(NonTermSymbol::new(tok.to_owned()))
    }

    fn parse_lex_symbols(&self, s: &str) -> Option<LexSymbol> {
        if let Some(sym) = self.term_token_regex(s) {
            return Some(LexSymbol::Term(sym));
        }

        if let Some(sym) = self.non_term_token_regex(s) {
            return Some(LexSymbol::NonTerm(sym));
        }

        None
    }

    /// Retrieve the next token. Read until the next character is not ASCII.
    /// TO DO: used only my start_directive!
    fn next_token(&self, i: usize) -> Option<(String, usize)> {
        let s_chars = self.tokens.as_slice();
        let j = self.ws(i)?; // j points to the non-ws
        let mut z = j;
        while z < s_chars.len() {
            let c = s_chars.get(z)?;
            if !c.is_ascii_alphabetic() {
                break;
            }
            z += 1;
        }

        let tok_chars = s_chars.get(j..z+1)?;
        let tok: String = tok_chars.iter().collect();

        Some((tok, z))
    }

    // HEADERS

    /// skip past %define directive, read until we encounter the next % sign
    /// otherwise, return the given index `i`.
    fn parse_define_directive(&self, i: usize) -> Option<usize> {
        let s_chars = self.tokens.as_slice();
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

    // TO DO: to be regex'd
    fn parse_start_directive(&mut self, i: usize) -> Option<usize> {
        let s_chars = self.tokens.as_slice();
        let mut j = i;
        // %start - look ahead next 5 symbols to check
        let peep = s_chars.get(j..j + 6)?;
        let peep_s: String = peep.iter().collect();
        if peep_s.eq("%start") {
            j += 6; //%start. root
            // start at . and read the next token, move the reference to after token
            let (tok, k) = self.next_token(j)?;
            j = k;
            self.add_start_symbol(tok);
        }

        Some(j)
    }

    /// Parse bison/YACC directives:
    /// - %define - marks the definition part
    /// - %start - marks the start rule part
    /// - %% - has two of these, marks the begin and end of rules section.
    fn parse_header_directives(&mut self, i: usize) -> Option<usize> {
        if let Some(mut j) = self.parse_define_directive(i) {
            if j > i {
                println!("%define, i:{} -> j:{}", i, j);
                j = self.parse_start_directive(j)
                    .expect("Unable to parse %start directive!");
                println!("%start, j:{}", j);
                j = self.rules_marker_directive(j)
                    .expect("Unable to parse %% directive at the end of header!");
                println!("%% matched, j:{}", j);
                return Some(j);
            }
        }

        Some(i)
    }

    /// check for `%%` directive; if so, move the pointer
    fn rules_marker_directive(&self, i: usize) -> Option<usize> {
        let s_chars = self.tokens.as_slice();
        let mut j = self.ws(i)?; // j points to the non-ws
        let peep = s_chars.get(j..j + 2)?;
        let s: String = peep.iter().collect();
        if s.eq("%%") {
            j += 2;
        }

        Some(j)
    }

    /// Parses the footer `%%` section, thus marking the end of parsing
    fn parse_footer_directive(&self, i: usize) -> Option<usize> {
        let j = self.rules_marker_directive(i)?;
        if j > i {
            println!("[END]");
            return Some(j);
        }

        Some(i)
    }

    // RULES

    /// read until `;`, otherwise we have reached end of rule section
    fn rule_end_marker(&self, i: usize) -> Option<usize> {
        let s_chars = self.tokens.as_slice();
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

    fn parse_alt(&self, s: &str) -> Option<Vec<LexSymbol>> {
        let tokens_s: Vec<&str> = s.split_ascii_whitespace().collect();
        let mut syms: Vec<LexSymbol> = vec![];
        for tok in tokens_s {
            let sym = self.parse_lex_symbols(tok)?;
            syms.push(sym);
        }

        Some(syms)
    }

    /// S: A 'b' C | 'x' | ;
    fn rule_rhs_regex(&self, s: &str) -> Option<Vec<RuleAlt>> {
        lazy_static! {
            static ref RE_ALT: Regex = Regex::new(r"(?P<alt>[a-zA-z'\s]+)")
            .expect("Unable to create regex for parsing rule alternatives");
            static ref RE_EMPTY_ALT: Regex = Regex::new(r"(?P<empty>\s*)")
            .expect("Unable to create regex to parse an empty alternative");
        }

        let alts: Vec<&str> = s.split('|').collect();
        let mut alts_s: Vec<RuleAlt> = vec![];
        for alt in alts {
            let alt_trimmed = alt.trim();
            match RE_ALT.captures(alt_trimmed) {
                Some(cap) => {
                    let alt_s = cap.name("alt")?.as_str();
                    let alts_syms = self.parse_alt(alt_s)?;
                    alts_s.push(RuleAlt::new(alts_syms));
                }
                _ => {
                    // try empty alt
                    let empty_alt_cap = RE_EMPTY_ALT.captures(alt)?;
                    let _ = empty_alt_cap.name("empty")?.as_str();
                    let alt_syms: Vec<LexSymbol> = vec![LexSymbol::Epsilon(EpsilonSymbol::new("".to_owned()))];
                    alts_s.push(RuleAlt::new(alt_syms));
                }
            }
        }

        Some(alts_s)
    }

    /// Parse the given string `s` and returs lhs and rhs of the rule.
    fn rule_regex<'a>(&self, s: &'a str) -> Option<(&'a str, &'a str)> {
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

    /// Create a rule
    /// From `i`, tries the rule end marker `;`, if found, returns the rule.
    /// Otherwise, returns `i`.
    fn rule(&self, i: usize) -> Option<(usize, Option<CfgRule>)> {
        let s_chars = self.tokens.as_slice();
        let j = self.rule_end_marker(i)?;
        if j > i {
            let rule = s_chars.get(i..j + 1)?;
            let rule_s: String = rule.iter().collect();
            let (lhs, rhs) = self.rule_regex(&rule_s)
                .expect("Unable to parse lhs, rhs of rule");
            let alts = self.rule_rhs_regex(rhs).unwrap();
            let cfg_rule = CfgRule::new(lhs.to_owned(), alts);

            return Some((j, Some(cfg_rule)));
        }

        Some((i, None))
    }

    /// Parses the rules section between the `%%` markers
    /// First read the root rule and then parse the rest of the rules
    fn parse_rules(&mut self, i: usize) -> Option<(usize, Vec<CfgRule>)> {
        println!("=> parsing rules");
        let s_chars = self.tokens.as_slice();
        let mut j = self.ws(i)?;
        let mut rules: Vec<CfgRule> = vec![];
        // start off with root rule
        let (k, root_rule) = self.rule(j)?;
        rules.push(root_rule.expect("No root rule!"));
        j = k;
        j += 1;
        while j < s_chars.len() {
            let (k, rule) = self.rule(j)?;
            if let Some(cfg_rule) = rule {
                rules.push(cfg_rule);
            }
            j = k;
            j += 1;
            let c = s_chars.get(j)?;
            // at the end of rule section
            if *c == '%' {
                break;
            }
        }

        Some((j, rules))
    }

    fn parse(&mut self) -> Cfg {
        let mut i = self.ws(0).expect("Unable to get first non-ws");
        i = self.parse_header_directives(i).expect("Unable to parse the header directives!");
        let (j, rules) = self.parse_rules(i).expect("Parsing of grammar rules failed!");
        self.parse_footer_directive(j).expect("Unable to parse footer directive");

        Cfg::new(rules)
    }
}

#[cfg(test)]
mod tests {
    use regex::Regex;
    use super::*;

    #[test]
    fn test_cfg() {
        let s = fs::read_to_string("./grammars/test.y")
            .expect("Unable to read grammar file");
        let s_chars: Vec<char> = s.chars().into_iter().collect();
        let mut cfg_parser = CfgParser::new(s_chars);
        let cfg = cfg_parser.parse();
        println!("cfg:\n{}", cfg);
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
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

    /// Parse bison/YACC directives:
    /// - %define - marks the definition part
    /// - %start - marks the start rule part
    /// - %% - has two of these, marks the begin and end of rules section.
    fn header_directives(&mut self, i: usize) -> Option<usize> {
        lazy_static! {
            static ref RE_HEADER: Regex = Regex::new(
            r"[\n\r\s]*%[a-zA-Z]+\s+([a-zA-Z\.]+)\s+([a-zA-Z\-]+)[\n\r\s]+%[a-zA-Z]+\s+(?P<start>[a-zA-Z]+)[\n\r\s]+%%"
            ).expect("Unable to create regex");
        }
        let s_chars = self.tokens.as_slice();
        let mut j = i;
        while j < s_chars.len() {
            let c = s_chars.get(j)?;
            println!("c: {}", c);
            if *c == '%' {
                // is the next char '%' too?
                let c_next = s_chars.get(j+1)?;
                if *c_next == '%' {
                    println!("reached %%");
                    // j+2 -- so we read until %%
                    let s: String = s_chars.get(i..j+2)?.iter().collect();
                    println!("header: {}", s);
                    let cap = RE_HEADER.captures(&s)?;
                    println!("header re capture: {:?}", cap);
                    let start_sym: &str = cap.name("start")?.as_str();
                    self.add_start_symbol(start_sym.to_owned());
                    return Some(j+2);
                }
            }
            j += 1;
        }

        Some(i)
    }

    // TO DO: REGEX it
    /// Parses the footer `%%` section, thus marking the end of parsing
    fn footer_tag(&self, i: usize) -> Option<bool> {
        lazy_static! {
            static ref RE_FOOTER: Regex = Regex::new( r"[\n\r\s]*%%")
            .expect("Unable to create regex");
        }
        let s_chars = self.tokens.as_slice();
        let peep = s_chars.get(i..)?;
        let s: String = peep.iter().collect();
        if let Some(_) = RE_FOOTER.captures(&s) {
            println!("[END]");
            return Some(true);
        }

        Some(false)
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
             static ref RE_RULE: Regex = Regex::new(r"[\n\r\s]+(?P<lhs>[a-zA-Z]+)[\s]*:(?P<rhs>[a-zA-z'|\s]+)[\s]*;")
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
        let mut j = i;
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
            // TO DO: if no rule found, then directly jump to footer section?
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
        let i = self.header_directives(0).expect("Unable to parse the header directives!");
        let (j, rules) = self.parse_rules(i).expect("Parsing of grammar rules failed!");
        self.footer_tag(j).expect("Unable to parse footer directive");

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
    fn test_header_regex() {
        let s = "%define lr.type canonical-lr\n%start root\n%%";
        // let re = Regex::new(r"(?P<lhs>[a-zA-Z]+):(?P<rhs>[a-zA-z'|\s]+)[\s]*;").expect("Unable to create regex");
        let re = Regex::new(
            r"%[a-zA-Z]+\s+([a-zA-Z\.]+)\s+([a-zA-Z\-]+)[\n\r\s]+%[a-zA-Z]+\s+(?P<startsym>[a-zA-Z]+)[\n\r\s]+%%"
        ).expect("Unable to create regex");
        let cap = re.captures(s)
            .expect("Unable to create capture");
        println!("cap: {:?}", cap);
    }
}
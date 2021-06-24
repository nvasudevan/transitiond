use std::{fs, io};
use regex;
use std::str::Chars;
use regex::Regex;

pub(crate) struct CfgParser {
    start_symbol: String,
    rules: Vec<String>,
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

    fn add_rule(&mut self, rule: String) {
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

// fn tok(s_char: &[char]) -> String {
//     let re = Regex::new(r"[[:alpha:]]")
//         .expect("Unable to create regex");
//
// }

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
fn parse_header_directives(s_chars: &[char], i: usize, mut cfg_parser: &mut CfgParser) -> Option<usize> {
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

/// Create a rule
fn rule(s_chars: &[char], i: usize, cfg_parser: &mut CfgParser) -> Option<usize> {
    let mut j = rule_end_marker(s_chars, i)?;
    if j > i {
        let rule = s_chars.get(i..j+1)?;
        let rule_s: String = rule.iter().collect();
        println!("rule: {}", rule_s);
        cfg_parser.rules.push(rule_s);
        return Some(j);
    }

    Some(i)
}

fn root_rule(s_chars: &[char], i: usize, mut cfg_parser: &mut CfgParser) -> Option<usize> {
    rule(s_chars, i, &mut cfg_parser)
}

fn parse_rules(s_chars: &[char], i: usize, mut cfg_parser: &mut CfgParser) -> Option<usize> {
    println!("=> parsing rules section...");
    let mut j = ws(s_chars, i)?;
    // first rule has to be the root rule
    j = root_rule(s_chars, j, &mut cfg_parser)?;
    j += 1;
    while j < s_chars.len() {
        println!("j: {}", j);
        j = rule(s_chars, j, &mut cfg_parser)?;
        j += 1;
        let c = s_chars.get(j)?;
        if *c == '%' {
            // at the end of rule section
           break
        }
    }

    Some(j)
}

fn read_file(cfgp: &str) -> io::Result<CfgParser> {
    let s = fs::read_to_string(cfgp)?;
    let mut s_chars: Vec<char> = s.chars().into_iter().collect();
    println!("[{}] s_chars: {:?}", s_chars.len(), s_chars);
    let mut cfg_parser = CfgParser::new();
    let mut i = ws(s_chars.as_slice(), 0)
        .expect("Unable to get first non-ws");
    i = parse_header_directives(s_chars.as_slice(), i, &mut cfg_parser)
        .expect("Unable to parse the header directives!");
    i = parse_rules(s_chars.as_slice(), i, &mut cfg_parser)
        .expect("Parsing of grammar rules failed!");
    i = parse_footer_directive(s_chars.as_slice(), i)
        .expect("Unable to parse footer directive");

    Ok(cfg_parser)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cfg() {
        let cfg_parser = read_file("./grammars/test.y").expect("xxx");
        println!("rules: \n{:?}", cfg_parser.rules);
    }
}
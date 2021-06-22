mod diagram;

use std::fmt;
use crate::cfg::diagram::{CfgGraph, Vertex};

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum SymType {
    NonTerminal,
    Terminal,
}

#[derive(Debug, Clone)]
pub(crate) struct NonTermSymbol {
    tok: String,
    tok_type: SymType,
}

impl fmt::Display for NonTermSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.tok)
    }
}

impl NonTermSymbol {
    pub(crate) fn new(tok: String) -> Self {
        Self {
            tok,
            tok_type: SymType::NonTerminal,
        }
    }
}

impl PartialEq for NonTermSymbol {
    fn eq(&self, other: &Self) -> bool {
        if self.tok_type.eq(&other.tok_type) && self.tok.eq(&other.tok) {
            return true;
        }

        false
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TermSymbol {
    tok: String,
    tok_type: SymType,
}

impl fmt::Display for TermSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", &self.tok)
    }
}

impl TermSymbol {
    pub(crate) fn new(tok: String) -> Self {
        Self {
            tok,
            tok_type: SymType::Terminal,
        }
    }
}

impl PartialEq for TermSymbol {
    fn eq(&self, other: &Self) -> bool {
        if self.tok_type.eq(&other.tok_type) && self.tok.eq(&other.tok) {
            return true;
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LexSymbol {
    NonTerm(NonTermSymbol),
    Term(TermSymbol),
}

impl fmt::Display for LexSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexSymbol::NonTerm(nt) => {
                nt.fmt(f)
            }
            LexSymbol::Term(term) => {
                term.fmt(f)
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct RuleAlt {
    lex_symbols: Vec<LexSymbol>,
}

impl fmt::Display for RuleAlt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        let mut alt_iter = self.lex_symbols.iter();
        if let Some(first_tok) = alt_iter.next() {
            s += first_tok.to_string().as_str();
            for tok in alt_iter {
                s = format!("{} {}", s, tok);
            }
        }
        write!(f, "{}", s)
    }
}

impl PartialEq for RuleAlt {
    fn eq(&self, other: &Self) -> bool {
        if self.to_string().eq(&other.to_string()) {
            return true;
        }

        false
    }
}

impl RuleAlt {
    pub(crate) fn new(lex_symbols: Vec<LexSymbol>) -> Self {
        Self {
            lex_symbols
        }
    }

    pub(crate) fn as_lrpar(&self) -> String {
        format!("{} {{ }}", self)
    }
}

#[derive(Debug)]
pub(crate) struct CfgRule {
    lhs: String,
    rhs: Vec<RuleAlt>,
}

impl fmt::Display for CfgRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut rhs_s = String::new();
        let mut rhs_iter = self.rhs.iter();
        if let Some(first_alt) = rhs_iter.next() {
            rhs_s += first_alt.to_string().as_str();
            for alt in rhs_iter {
                rhs_s = format!("{} | {}", rhs_s, alt.to_string())
            }
        }
        let s = format!("{}: {}", self.lhs, rhs_s);
        write!(f, "{}", s)
    }
}

impl CfgRule {
    pub(crate) fn new(lhs: String, rhs: Vec<RuleAlt>) -> Self {
        Self {
            lhs,
            rhs,
        }
    }

    pub(crate) fn as_lrpar(&self) -> String {
        let alts_s: Vec<String> = self.rhs.iter().
            map(|alt| alt.as_lrpar())
            .collect();
        let rhs_s = alts_s.join(" | ");

        format!("{} ->: {}", self.lhs, rhs_s)
    }
}

#[derive(Debug)]
pub(crate) struct Cfg {
    rules: Vec<CfgRule>,
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        let mut rules_iter = self.rules.iter();
        if let Some(start_rule) = rules_iter.next() {
            s = format!("{}\n;\n", start_rule);
            for rule in rules_iter {
                s = format!("{}{}\n;\n", s, rule);
            }
        }
        write!(f, "{}", s)
    }
}

impl Cfg {
    pub(crate) fn new(rules: Vec<CfgRule>) -> Self {
        Self {
            rules
        }
    }

    pub(crate) fn start_rule(&self) -> Option<&CfgRule> {
        self.rules.first()
    }

    pub(crate) fn as_hyacc(&self) -> String {
        let s_rule = self.start_rule()
            .expect("Cfg is missing a start rule!");

        format!("%start {}\n\n%%\n\n{}\n\n%%", s_rule.lhs, self)
    }

    pub(crate) fn as_yacc(&self) -> String {
        format!("%define lr.type canonical-lr\n\n{}", self.as_hyacc())
    }

    pub(crate) fn as_lrpar(&self) -> String {
        let s_rule = self.start_rule()
            .expect("Cfg is missing a start rule!");

        let mut s = String::new();
        for rule in &self.rules {
            s = format!("{}{}\n;\n", s, rule.as_lrpar());
        }

        format!("%start {}\n\n%%\n\n{}\n\n%%", s_rule.lhs, s)
    }

    pub(crate) fn terminals_only_alts(&self) -> Vec<(String, &RuleAlt)> {
        let mut terms_only_alts = Vec::<(String, &RuleAlt)>::new();
        for rule in &self.rules {
            for alt in &rule.rhs {
                let mut has_non_term = false;
                for sym in &alt.lex_symbols {
                    if let LexSymbol::NonTerm(_) = sym {
                        has_non_term = true;
                        break
                    }
                }
                if !has_non_term {
                    terms_only_alts.push((rule.lhs.to_owned(), alt));
                }
            }
        }
        terms_only_alts
    }
}

// impl Into<CfgGraph> for Cfg {
//     fn into(self) -> CfgGraph {
//         let mut graph = CfgGraph::new();
//         let terms_only_rules = self.terminals_only_alts();
//         let mut vertices: Vec<Vertex> = self.rules
//             .iter()
//             .map(|r| Vertex::new(format!("u_{}", r.lhs)))
//             .collect();
//
//         let mut v_vertices: Vec<Vertex> = self.rules
//             .iter()
//             .map(|r| Vertex::new(format!("v_{}", r.lhs)))
//             .collect();
//
//         vertices.append(&mut v_vertices);
//         graph.set_vertices(vertices);
//
//         graph
//     }
// }

#[cfg(test)]
mod tests {
    use super::{LexSymbol, NonTermSymbol, TermSymbol};
    use super::RuleAlt;
    use crate::cfg::{CfgRule, Cfg};
    use crate::cfg::diagram::{CfgGraph, Edge};

    fn simple_cfg() -> Cfg {
        let mut rules: Vec<CfgRule> = vec![];

        // S: F B 'x' | G B 'y'
        let s_lhs = "S".to_string();
        let mut alt1_syms = Vec::<LexSymbol>::new();
        alt1_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("F".to_string())));
        alt1_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("B".to_string())));
        alt1_syms.push(LexSymbol::Term(TermSymbol::new("x".to_string())));
        let s_alt1 = RuleAlt::new(alt1_syms);

        let mut alt2_syms = Vec::<LexSymbol>::new();
        alt2_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("G".to_string())));
        alt2_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("B".to_string())));
        alt2_syms.push(LexSymbol::Term(TermSymbol::new("y".to_string())));
        let s_alt2 = RuleAlt::new(alt2_syms);

        let s_rhs = vec![s_alt1, s_alt2];
        rules.push(CfgRule::new(s_lhs, s_rhs));

        // F: 'a'
        let mut f_alt_syms = Vec::<LexSymbol>::new();
        f_alt_syms.push(LexSymbol::Term(TermSymbol::new("a".to_string())));
        let f_alt1 = RuleAlt::new(f_alt_syms);
        rules.push(CfgRule::new("F".to_string(), vec![f_alt1]));

        // G: 'a'
        let mut g_alt_syms = Vec::<LexSymbol>::new();
        g_alt_syms.push(LexSymbol::Term(TermSymbol::new("a".to_string())));
        let g_alt1 = RuleAlt::new(g_alt_syms);
        rules.push(CfgRule::new("G".to_string(), vec![g_alt1]));

        // B: 'b' 'b'
        let mut b_alt_syms = Vec::<LexSymbol>::new();
        b_alt_syms.push(LexSymbol::Term(TermSymbol::new("b".to_string())));
        b_alt_syms.push(LexSymbol::Term(TermSymbol::new("b".to_string())));
        let b_alt1 = RuleAlt::new(b_alt_syms);
        rules.push(CfgRule::new("B".to_string(), vec![b_alt1]));

        Cfg::new(rules)
    }

    #[test]
    fn test_cfg() {
        let cfg = simple_cfg();

        let mut graph = CfgGraph::from(cfg);
        for v in graph.vertices() {
            println!("v: {:?}", v);
        }

        for e in graph.edges() {
            println!("e: {:?}", e);
        }

    }

}
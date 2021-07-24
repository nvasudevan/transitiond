use std::fmt;
use std::convert::TryFrom;

pub(crate) mod diagram;
pub(crate) mod parse;
pub(crate) mod graph;
pub(crate) mod mutate;
pub(crate) mod dataset;

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum SymType {
    NonTerminal,
    Terminal,
    Epsilon,
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

#[derive(Debug, Clone)]
pub(crate) struct EpsilonSymbol {
    tok_type: SymType,
}

impl fmt::Display for EpsilonSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "<eps>")
    }
}

impl EpsilonSymbol {
    pub(crate) fn new() -> Self {
        Self {
            tok_type: SymType::Epsilon,
        }
    }
}

impl PartialEq for EpsilonSymbol {
    fn eq(&self, other: &Self) -> bool {
        if self.tok_type.eq(&other.tok_type) {
            return true;
        }

        false
    }
}

/// LexSymbol can be of three types:
/// - a non-terminal (`NonTerm`)
/// - a terminal (`Term`)
/// - an epsilon (`Epsilon`)
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LexSymbol {
    NonTerm(NonTermSymbol),
    Term(TermSymbol),
    Epsilon(EpsilonSymbol),
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
            LexSymbol::Epsilon(eps) => {
                eps.fmt(f)
            }
        }
    }
}

impl LexSymbol {
    fn to_term(&self) -> Option<&TermSymbol> {
        match self {
            LexSymbol::Term(t) => {
                Some(t)
            },
            _ => { None }
        }
    }
}

impl TryFrom<LexSymbol> for TermSymbol {
    type Error = ();

    fn try_from(value: LexSymbol) -> Result<Self, Self::Error> {
        match value {
            LexSymbol::Term(t) => { Ok(t) },
            _ => { Err(()) }
        }
    }
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
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

impl PartialEq for CfgRule {
    fn eq(&self, other: &Self) -> bool {
        if self.to_string().eq(&other.to_string()) {
            return true;
        }
        false
    }
}

impl CfgRule {
    pub(crate) fn new(lhs: String, rhs: Vec<RuleAlt>) -> Self {
        Self {
            lhs,
            rhs,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Cfg {
    rules: Vec<CfgRule>,
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        let mut rules_iter = self.rules.iter();
        if let Some(start_rule) = rules_iter.next() {
            s = format!("{}\n\n{}\n;\n", s, start_rule);
            for rule in rules_iter {
                s = format!("{}{}\n;\n", s, rule);
            }
        }
        write!(f, "{}", s)
    }
}

impl PartialEq for Cfg {
    fn eq(&self, other: &Self) -> bool {
        if self.to_string().eq(&other.to_string()) {
            return true;
        }
        false
    }
}

impl Cfg {
    pub(crate) fn new(rules: Vec<CfgRule>) -> Self {
        Self {
            rules
        }
    }

    pub(crate) fn as_yacc(&self) -> String {
        let start_rule = self.get_rule("root")
            .expect("No start rule found!");
        let mut s = format!("%define lr.type canonical-lr\n%start {}\n%%",
                            start_rule.lhs);
        s = format!("{}\n{}", s, self);

        format!("{}\n\n%%\n", s)
    }

    pub(crate) fn terminals(&self) -> Vec<&TermSymbol> {
        let mut terms: Vec<&TermSymbol> = vec![];
        for rule in &self.rules {
            for alt in &rule.rhs {
                for sym in &alt.lex_symbols {
                    if let LexSymbol::Term(t) = sym {
                        if ! terms.contains(&t) {
                            terms.push(t);
                        }
                    }
                }
            }
        }

        terms
    }

    pub(crate) fn get_rule(&self, name: &str) -> Option<&CfgRule> {
        for r in &self.rules {
            if r.lhs.eq(name) {
                return Some(r);
            }
        }

        None
    }

    pub(crate) fn get_alt_mut(&mut self, name: &str, alt_index: usize) -> Option<&mut RuleAlt> {
        for r in &mut self.rules {
            if r.lhs.eq(name) {
                let alt = &mut r.rhs[alt_index];
                return Some(alt);
            }
        }

        None
    }

    pub(crate) fn terminals_only_alts(&self) -> Vec<(&str, &RuleAlt)> {
        let mut alts = Vec::<(&str, &RuleAlt)>::new();
        for rule in &self.rules {
            for alt in &rule.rhs {
                let mut has_non_term = false;
                for sym in &alt.lex_symbols {
                    if let LexSymbol::NonTerm(_) = sym {
                        has_non_term = true;
                        break;
                    }
                }
                if !has_non_term {
                    alts.push((rule.lhs.as_str(), alt));
                }
            }
        }
        alts
    }

    /// Returns alternatives which start with terminals and has a non-terminal
    pub(crate) fn alt_start_with_terminals(&self) -> Vec<(&str, &RuleAlt, i8)> {
        let mut alts = Vec::<(&str, &RuleAlt, i8)>::new();
        for rule in &self.rules {
            for alt in &rule.rhs {
                if let Some(LexSymbol::Term(_)) = alt.lex_symbols.first() {
                    let mut has_non_term = false;
                    let mut nt_i: i8 = -1;
                    if let Some(more_syms) = alt.lex_symbols.get(1..) {
                        for (i, sym) in more_syms.iter().enumerate() {
                            if let LexSymbol::NonTerm(_) = sym {
                                has_non_term = true;
                                nt_i = (i + 1) as i8;
                                break;
                            }
                        }
                    }
                    if has_non_term {
                        alts.push((rule.lhs.as_str(), alt, nt_i));
                    }
                }
            }
        }

        alts
    }

    pub(crate) fn alt_start_with_non_terminals(&self) -> Vec<(&str, &RuleAlt)> {
        let mut alts = Vec::<(&str, &RuleAlt)>::new();
        for rule in &self.rules {
            for alt in &rule.rhs {
                if let Some(LexSymbol::NonTerm(_)) = alt.lex_symbols.first() {
                    alts.push((rule.lhs.as_str(), alt));
                    break;
                }
            }
        }
        alts
    }
}
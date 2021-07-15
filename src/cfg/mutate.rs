use crate::cfg::{Cfg, LexSymbol, TermSymbol, RuleAlt};
use std::collections::HashMap;
use rand;
use rand::thread_rng;
use rand::prelude::SliceRandom;

fn terminals(cfg: &Cfg) -> Vec<TermSymbol> {
    let mut terms: Vec<TermSymbol> = vec![];
    for rule in &cfg.rules {
        for alt in &rule.rhs {
            for sym in &alt.lex_symbols {
                if let LexSymbol::Term(t) = sym {
                    if ! terms.contains(t) {
                        terms.push(t.clone());
                    }
                }
            }
        }
    }

    terms
}

fn terminal_alts(cfg: &Cfg) -> HashMap<String, Vec<usize>> {
    let mut term_alts: HashMap<String, Vec<usize>> = HashMap::new();
    for rule in &cfg.rules {
        let mut term_indices: Vec<usize> = vec![];
        for (i, alt) in rule.rhs.iter().enumerate() {
            let mut has_terms = false;
            for sym in &alt.lex_symbols {
                if let LexSymbol::Term(_) = sym {
                    has_terms = true;
                    break
                }
            }
            if has_terms {
                term_indices.push(i);
            }
        }
        if ! term_indices.is_empty() {
            term_alts.insert(rule.lhs.to_owned(), term_indices);
        }
    }

    term_alts
}

/// Mutates the give alternative using the terminals `terms`
fn mutate_alt(alt: &mut RuleAlt, terms: &Vec<TermSymbol>) {
    //
}

fn mutate(base_cfg: &Cfg, terminal_alts: &HashMap<String, Vec<usize>>, terms: &Vec<TermSymbol>) -> Option<Cfg> {
    let mut cfg = base_cfg.clone();
    let non_terms: Vec<&String> = terminal_alts.keys().collect();
    let nt = non_terms.choose(&mut thread_rng())?;
    let nt_indices = terminal_alts.get(*nt)?;
    let nt_alt_i = nt_indices.choose(&mut thread_rng())?;
    let mut nt_alt = cfg.get_alt_mut(*nt, *nt_alt_i)?;
    println!("k: {}, alt: {}", nt, nt_alt);
    mutate_alt(&mut nt_alt, terms);

    Some(cfg)
}

#[cfg(test)]
mod tests {
    use crate::cfg::parse;
    use crate::cfg::mutate::{terminals, terminal_alts, mutate};

    #[test]
    fn test_single_mutation() {
        let cfg = parse::parse("./grammars/lr1.y")
            .expect("Unable to parse as a cfg");
        let terms = terminals(&cfg);
        println!("terms: {:?}", terms);
        let terminal_alts = terminal_alts(&cfg);
        println!("terminal alts: {:?}", terminal_alts);
        mutate(&cfg, &terminal_alts, &terms);
    }
}
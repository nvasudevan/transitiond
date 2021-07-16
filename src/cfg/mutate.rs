use std::collections::HashMap;

use rand;
use rand::prelude::SliceRandom;
use rand::thread_rng;

use crate::cfg::{Cfg, LexSymbol, RuleAlt, TermSymbol};

#[derive(Debug)]
pub(crate) struct CfgMutateError {
    msg: String,
}

impl CfgMutateError {
    fn new(msg: String) -> Self {
        CfgMutateError {
            msg
        }
    }
}

fn terminal_alts_indices(cfg: &Cfg) -> HashMap<String, Vec<usize>> {
    let mut term_alts: HashMap<String, Vec<usize>> = HashMap::new();
    for rule in &cfg.rules {
        let mut term_indices: Vec<usize> = vec![];
        for (i, alt) in rule.rhs.iter().enumerate() {
            let mut has_terms = false;
            for sym in &alt.lex_symbols {
                if let LexSymbol::Term(_) = sym {
                    has_terms = true;
                    break;
                }
            }
            if has_terms {
                term_indices.push(i);
            }
        }
        if !term_indices.is_empty() {
            term_alts.insert(rule.lhs.to_owned(), term_indices);
        }
    }

    term_alts
}

/// Mutates the give alternative using the base set of terminals `terms`
fn mutate_alt(alt: &mut RuleAlt, terms: &Vec<&TermSymbol>) -> Result<(), CfgMutateError> {
    let mut indices: Vec<usize> = vec![];
    for (i, sym) in alt.lex_symbols.iter().enumerate() {
        if let LexSymbol::Term(_) = sym {
            indices.push(i);
        }
    }
    let j = indices.choose(&mut thread_rng()).unwrap();
    let mut_sym = &alt.lex_symbols[*j];
    let term_exclude = LexSymbol::to_term(mut_sym)
        .ok_or_else(|| CfgMutateError::new(
            format!("Unable to convert LexSymbol {} to TermSymbol", mut_sym)
        ))?;

    let new_terms: Vec<&TermSymbol> = terms
        .iter()
        .filter(|t| (**t).ne(term_exclude))
        .map(|t| *t)
        .collect();

    let new_term = new_terms.choose(&mut thread_rng()).unwrap();
    println!("t_exclude: {}, new_term: {}", term_exclude, new_term);
    alt.lex_symbols[*j] = LexSymbol::Term((*new_term).clone());

    Ok(())
}

fn run(base_cfg: &Cfg, cnt: usize) -> Result<Vec<Cfg>, CfgMutateError> {
    let terminal_alts = terminal_alts_indices(base_cfg);
    let non_terms: Vec<&String> = terminal_alts.keys().collect();
    let terms = base_cfg.terminals();

    let mut mutated_cfgs: Vec<Cfg> = vec![];
    let mut i: usize = 0;
    loop {
        let nt = non_terms.choose(&mut thread_rng()).unwrap();
        let nt_indices = terminal_alts.get(*nt).unwrap();
        let nt_alt_i = nt_indices.choose(&mut thread_rng()).unwrap();
        let mut cfg = base_cfg.clone();
        let mut nt_alt = cfg.get_alt_mut(*nt, *nt_alt_i)
        .ok_or_else(||
            CfgMutateError::new(
                format!("Failed to get alternative for non-terminal {} (index: {})",
                        nt,
                        nt_alt_i
                )
            ))?;
        mutate_alt(&mut nt_alt, &terms)?;
        println!("alt: {}: {}", nt, nt_alt);
        println!("cfg: {}", cfg);
        mutated_cfgs.push(cfg);

        i += 1;
        if i >= cnt {
            break;
        }
    }

    Ok(mutated_cfgs)
}

#[cfg(test)]
mod tests {
    use crate::cfg::mutate::run;
    use crate::cfg::parse;

    #[test]
    fn test_single_mutation() {
        let cfg = parse::parse("./grammars/lr1.y")
            .expect("Unable to parse as a cfg");
        let cnt: usize = 3;
        let _ = run(&cfg, cnt)
            .expect("Unable to generate a mutated cfg");
    }
}
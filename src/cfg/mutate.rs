use std::collections::HashMap;

use rand;
use rand::prelude::SliceRandom;
use rand::thread_rng;

use crate::cfg::{Cfg, LexSymbol, TermSymbol};
use std::io::Write;

const MAX_ITER_LIMIT: usize = 500;

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

pub(crate) struct CfgMutation<'a> {
    cfg: &'a Cfg,
    terminal_indices: HashMap<String, Vec<Vec<usize>>>,
    non_terms: Vec<String>,
    pub(crate) terms: Vec<&'a TermSymbol>,
}

impl<'a> CfgMutation<'a> {
    pub(crate) fn new(cfg: &'a Cfg) -> Self {
        Self {
            cfg,
            terminal_indices: Default::default(),
            non_terms: vec![],
            terms: vec![],
        }
    }

    /// Calculates the indices of alternatives for each rule
    fn terminal_indices(&mut self) {
        let mut term_indices_map: HashMap<String, Vec<Vec<usize>>> = HashMap::new();
        for rule in &self.cfg.rules {
            let mut rule_i: Vec<Vec<usize>> = vec![];
            for alt in &rule.rhs {
                let mut alt_i: Vec<usize> = vec![];
                for (j, sym) in alt.lex_symbols.iter().enumerate() {
                    if let LexSymbol::Term(_) = sym {
                        alt_i.push(j);
                    }
                }
                rule_i.push(alt_i);
            }
            term_indices_map.insert(rule.lhs.to_owned(), rule_i);
        }

        self.terminal_indices = term_indices_map;
    }

    /// Returns a list of non-terminals which have alternatives with terminals in them
    fn nt_alt_with_terminals(&mut self) {
        let mut keys: Vec<String> = vec![];
        for (k, values) in &self.terminal_indices {
            for v in values {
                if v.len() > 0 {
                    keys.push(k.to_owned());
                    break;
                }
            }
        }
        self.non_terms = keys;
    }

    pub(crate) fn instantiate(&mut self) {
        self.terminal_indices();
        self.nt_alt_with_terminals();
        self.terms = self.cfg.terminals();
    }

    /// Returns the number of possible mutations where terminals occur
    pub(crate) fn mut_cnt(&self) -> usize {
        self.terminal_indices.values()
            .map(|u|
                u.iter().map(|v| v.len()).sum::<usize>()
            )
            .sum::<usize>()
    }

    /// Returns a tuple of indices of terminal location
    fn alt_with_terminals(&self, nt: &str) -> (usize, usize) {
        let alt_with_terms = self.terminal_indices.get(nt).unwrap();
        let mut alt_indices: Vec<usize> = vec![];
        for (i, v) in alt_with_terms.iter().enumerate() {
            if v.len() > 0 {
                alt_indices.push(i);
            }
        }
        let j = alt_indices.choose(&mut thread_rng()).unwrap();
        let terminal_indices = &alt_with_terms[*j];
        let terminal_i = terminal_indices.choose(&mut thread_rng()).unwrap();
        // println!("{}: alt_with_terms: {:?}, j: {}, term_i: {}", nt, alt_with_terms, j, terminal_i);

        (*j, *terminal_i)
    }

    /// Mutates the give alternative using the base set of terminals `terms`
    fn mutate(&mut self) -> Result<Cfg, CfgMutateError> {
        let nt = self.non_terms.choose(&mut thread_rng()).unwrap();
        let (alt_i, term_j) = self.alt_with_terminals(&nt);
        let mut cfg = self.cfg.clone();
        let alt = cfg.get_alt_mut(nt, alt_i)
            .ok_or_else(||
                CfgMutateError::new(
                    format!("Failed to get alternative for non-terminal {} (index: {})",
                            nt,
                            alt_i
                    )
                ))?;
        let mut_sym = &alt.lex_symbols[term_j];
        let term_exclude = LexSymbol::to_term(mut_sym)
            .ok_or_else(|| CfgMutateError::new(
                format!("Unable to convert LexSymbol {} to TermSymbol", mut_sym)
            ))?;

        let new_terms: Vec<&TermSymbol> = self.terms
            .iter()
            .filter(|t| (**t).ne(term_exclude))
            .map(|t| *t)
            .collect();
        let new_term = new_terms.choose(&mut thread_rng()).unwrap();
        alt.lex_symbols[term_j] = LexSymbol::Term((*new_term).clone());

        Ok(cfg)
    }
}

/// Start a mutation run until we generate the `cnt` mutated grammars
/// or hit the `MAX_ITER_LIMIT` threshold.
pub(crate) fn run(cfg_mut: &mut CfgMutation, cnt: usize) -> Result<Vec<Cfg>, CfgMutateError> {
    let mut mutated_cfgs: Vec<Cfg> = vec![];
    let mut i: usize = 0;
    loop {
        let cfg = cfg_mut.mutate()?;
        if !mutated_cfgs.contains(&cfg) {
            mutated_cfgs.push(cfg);
            eprint!(".");
        } else {
            eprint!("X");
        }
        std::io::stdout().flush().unwrap();

        i += 1;
        if (i >= MAX_ITER_LIMIT) || (mutated_cfgs.len() >= cnt) {
            break;
        }
    }

    Ok(mutated_cfgs)
}

#[cfg(test)]
mod tests {
    use crate::cfg::mutate::{run, CfgMutation};
    use crate::cfg::parse;
    use std::path::Path;
    use crate::cfg::graph::graph;

    #[test]
    fn test_cfg_mutation() {
        let cfg = parse::parse("./grammars/lr1.y")
            .expect("Unable to parse as a cfg");
        let mut cfg_mut = CfgMutation::new(&cfg);
        cfg_mut.instantiate();
        let cnt = cfg_mut.mut_cnt() * (cfg_mut.terms.len() - 1);
        let cfgs = run(&mut cfg_mut, cnt)
            .expect("Unable to generate a mutated cfg");
        println!("\n=> generated {} cfgs, writing ...", cfgs.len());
        let basep = Path::new("/var/tmp/cfgtest");

        for (i, cfg) in cfgs.iter().enumerate() {
            let cfgp = basep.join(i.to_string());
            std::fs::write(&cfgp, cfg.as_yacc())
                .expect(&format!("Failed to write cfg {}", cfg));
            let g = graph(cfgp.to_str().unwrap())
                .expect("Unable to create graph");
            let g_result = g.instantiate()
                .expect("Unable to convert cfg to graph");
            println!("=>graph: {}", g_result);
        }
    }
}
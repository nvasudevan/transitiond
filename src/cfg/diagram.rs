use std::{
    fmt,
    rc::Rc,
};

use crate::cfg::{Cfg, EpsilonSymbol, LexSymbol, parse};
use crate::cfg::parse::CfgParseError;

/// Represents a Cfg graph node/vertex
#[derive(Debug, Clone)]
pub(crate) struct TransitionNode {
    /// the label of the node
    label: String,
}

impl TransitionNode {
    fn new(label: String) -> Self {
        Self {
            label
        }
    }
}

impl fmt::Display for TransitionNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("[{}]", self.label.as_str()))
    }
}

impl PartialEq for TransitionNode {
    fn eq(&self, other: &Self) -> bool {
        self.label.eq(&other.label)
    }
}

/// An `Edge` represents a derivation where a set of symbols have been consumed.
#[derive(Debug)]
pub(crate) struct TransitionEdge {
    /// start vertex of the edge
    source: Rc<TransitionNode>,
    /// end vertex of the edge
    target: Rc<TransitionNode>,
    /// terminals consumed
    derived: Vec<LexSymbol>,
    /// symbols yet to be derived
    to_be_derived: Vec<LexSymbol>,
}

impl TransitionEdge {
    pub(crate) fn new(source: Rc<TransitionNode>,
                      target: Rc<TransitionNode>,
                      terminals: Vec<LexSymbol>,
                      to_be_derived: Vec<LexSymbol>) -> Self {
        Self {
            source,
            target,
            derived: terminals,
            to_be_derived,
        }
    }
}

impl fmt::Display for TransitionEdge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let derived_s: Vec<String> = self.derived
            .iter()
            .map(|x| x.to_string())
            .collect();
        let to_be_derived: Vec<String> = self.to_be_derived
            .iter()
            .map(|x| x.to_string())
            .collect();
        let s = format!("{} --> {} <({}),({})>",
                        self.source,
                        self.target,
                        derived_s.join(","),
                        to_be_derived.join(","));
        write!(f, "{}", s)
    }
}

/// Represents a CFG graph
#[derive(Debug)]
pub struct TransitionGraph {
    cfg: Cfg,
    nodes: Vec<Rc<TransitionNode>>,
    edges: Vec<TransitionEdge>,
}

impl TransitionGraph {
    fn new(cfg: Cfg) -> Self {
        // build start nodes (u_) ones
        let mut nodes: Vec<Rc<TransitionNode>> = cfg.rules
            .iter()
            .map(|r| Rc::new(TransitionNode::new(format!("u_{}", r.lhs))))
            .collect();

        let mut v_nodes: Vec<Rc<TransitionNode>> = cfg.rules
            .iter()
            .map(|r| Rc::new(TransitionNode::new(format!("v_{}", r.lhs))))
            .collect();

        nodes.append(&mut v_nodes);

        Self {
            cfg,
            nodes,
            edges: vec![],
        }
    }

    fn find_vertex_by_label(&self, label: &str) -> Option<&Rc<TransitionNode>> {
        for v in &self.nodes {
            if v.label.eq(label) {
                return Some(v);
            }
        }
        None
    }

    /// Build edges for alternatives with terminals only (e.g. A: 'x' 'y')
    fn build_terminal_only_edges(&mut self) -> Option<()> {
        let alts = self.cfg.terminals_only_alts();
        let mut edges = Vec::<TransitionEdge>::new();
        for (lhs, alt) in alts {
            let start_label = format!("u_{}", lhs);
            let end_label = format!("v_{}", lhs);
            let start_v = self.find_vertex_by_label(&start_label)?;
            let end_v = self.find_vertex_by_label(&end_label)?;
            let derived = &alt.lex_symbols;
            let to_be_derived: Vec<LexSymbol> = vec![
                LexSymbol::Epsilon(EpsilonSymbol::new())
            ];
            edges.push(
                TransitionEdge::new(
                    Rc::clone(&start_v),
                    Rc::clone(&end_v),
                    derived.clone(),
                    to_be_derived,
                )
            );
        }
        self.edges.append(&mut edges);

        Some(())
    }

    /// build edges from alternatives that begin with a terminal:
    ///  - `A: 'x' B`, edge: `u_A -> u_B <('x'), (B)>`
    fn build_start_terminal_edges(&mut self) -> Option<()> {
        let alts = self.cfg.alt_start_with_terminals();
        let mut edges = Vec::<TransitionEdge>::new();
        for (lhs, alt, nt_i) in alts {
            let derived: &[LexSymbol] = alt.lex_symbols.get(0..nt_i as usize)?;
            let to_be_derived = alt.lex_symbols.get((nt_i as usize)..)?;
            let start_label = format!("u_{}", lhs);
            let start_v = self.find_vertex_by_label(&start_label)?;

            // use the first non-terminal from to_be_derived as the destination
            let nt_node = to_be_derived.first()?;
            let end_label = format!("v_{}", nt_node.to_string());
            let end_v = self.find_vertex_by_label(&end_label.to_string())?;
            edges.push(
                TransitionEdge::new(
                    Rc::clone(&start_v),
                    Rc::clone(&end_v),
                    derived.to_vec(),
                    to_be_derived.to_vec(),
                )
            );
        }
        self.edges.append(&mut edges);

        Some(())
    }

    /// build edges from alternatives that begin with a non-terminal:
    /// - `A: C 'x'`, edge: `u_A -> u_C <('<eps>'), (C, 'x'))`
    fn build_start_non_terminal_edges(&mut self) -> Option<()> {
        let alts = self.cfg.alt_start_with_non_terminals();
        let mut edges = Vec::<TransitionEdge>::new();
        for (lhs, alt) in alts {
            let start_label = format!("u_{}", lhs);
            let start_v = self.find_vertex_by_label(&start_label)?;

            // use the first non-terminal of the alt as the destination
            let nt_node = alt.lex_symbols.first()?;
            let end_label = format!("u_{}", nt_node.to_string());
            let end_v = self.find_vertex_by_label(&end_label.to_string())?;

            let derived: Vec<LexSymbol> = vec![
                LexSymbol::Epsilon(EpsilonSymbol::new())
            ];
            let to_be_derived = &alt.lex_symbols;
            edges.push(
                TransitionEdge::new(
                    Rc::clone(&start_v),
                    Rc::clone(&end_v),
                    derived,
                    to_be_derived.clone(),
                )
            );
        }
        self.edges.append(&mut edges);
        Some(())
    }

    /// Build edges of the graph
    /// 1. build the terminal only edges (e.g. A: 'a')
    fn build_edges(&mut self) -> Option<()> {
        self.build_terminal_only_edges()?;
        self.build_start_terminal_edges()?;
        self.build_start_non_terminal_edges()?;

        Some(())
    }
}

pub(crate) fn graph(cfgp: &str) -> Result<TransitionGraph, CfgParseError> {
    let cfg = parse::parse(cfgp)?;
    println!("cfg:\n{}", cfg);
    let mut graph = TransitionGraph::new(cfg);
    graph.build_edges();

    Ok(graph)
}

#[cfg(test)]
mod tests {
    use crate::cfg::{LexSymbol, NonTermSymbol, TermSymbol};
    use crate::cfg::{Cfg, CfgRule, RuleAlt};
    use crate::cfg::diagram::{TransitionGraph, graph};

    fn simple_cfg() -> Cfg {
        let mut rules: Vec<CfgRule> = vec![];

        // S: F B 'x' | 'v' 'v' G B 'y'
        let s_lhs = "S".to_string();
        let mut alt1_syms = Vec::<LexSymbol>::new();
        alt1_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("F".to_string())));
        alt1_syms.push(LexSymbol::NonTerm(NonTermSymbol::new("B".to_string())));
        alt1_syms.push(LexSymbol::Term(TermSymbol::new("x".to_string())));
        let s_alt1 = RuleAlt::new(alt1_syms);

        let mut alt2_syms = Vec::<LexSymbol>::new();
        alt2_syms.push(LexSymbol::Term(TermSymbol::new("v".to_string())));
        alt2_syms.push(LexSymbol::Term(TermSymbol::new("v".to_string())));
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
        println!("cfg:\n{}", cfg);

        let mut graph = TransitionGraph::new(cfg);
        graph.build_edges();

        for e in graph.edges {
            println!("e: {}", e);
        }
    }

    #[test]
    fn test_cfg_from_file() {
        let g = graph("./grammars/test.y")
            .expect("grammar parse failed");
        for e in g.edges {
            println!("e: {}", e);
        }
    }
}

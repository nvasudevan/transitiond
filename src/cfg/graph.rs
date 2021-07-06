use std::{
    fmt,
    rc::Rc,
};

use crate::cfg::{Cfg, EpsilonSymbol, LexSymbol, NonTermSymbol, parse, TermSymbol};
use crate::cfg::parse::CfgParseError;

/// Represents a Cfg graph node/vertex
#[derive(Debug, Clone)]
pub(crate) struct Node {
    /// the non-terminal rule name
    pub(crate) lhs: String,
    /// the label of the node
    pub(crate) item: Vec<LexSymbol>,
    // index of the symbol to be derived next
    pub(crate) index: usize,
}

impl Node {
    /// index points to the index of the dot
    pub(crate) fn new(lhs: String, item: Vec<LexSymbol>, index: usize) -> Self {
        Self {
            lhs,
            item,
            index,
        }
    }

    pub(crate) fn set_index(&mut self, index: usize) {
        self.index = index;
    }

    pub(crate) fn next(&self, i: usize) -> Option<&LexSymbol> {
        self.item.get(i)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pre = self.item.get(0..self.index)
            .expect("fail to retrieve items");
        let pre_s: Vec<String> = pre.iter()
            .map(|x| x.to_string()).collect();
        let post = self.item.get(self.index..)
            .expect("fail to retrieve items");
        let post_s: Vec<String> = post.iter()
            .map(|x| x.to_string()).collect();

        write!(f, "{}", format!("[{}: {}.{}]",
                                self.lhs,
                                pre_s.join(" "),
                                post_s.join(" "))
        )
    }
}

#[derive(Debug)]
pub(crate) enum EdgeType {
    Derive,
    Shift,
    Reduce,
}

/// An `Edge` represents a derivation where a symbol has been consumed.
#[derive(Debug)]
pub(crate) struct Edge {
    /// source node
    source: Rc<Node>,
    /// target node
    target: Rc<Node>,
    /// symbol consumed
    derived: LexSymbol,
    /// Edge type: derive or reduce
    edge_type: EdgeType,
}

impl Edge {
    pub(crate) fn new(source: Rc<Node>,
                      target: Rc<Node>,
                      derived: LexSymbol,
                      edge_type: EdgeType) -> Self {
        Self {
            source,
            target,
            derived,
            edge_type,
        }
    }
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self.edge_type {
            EdgeType::Derive => {
                format!("{} --> {}", self.source, self.target)
            }
            EdgeType::Shift => {
                format!("{} --<{}>--> {}", self.source, self.derived, self.target)
            }
            EdgeType::Reduce => {
                format!("{} ==> {}", self.source, self.target)
            }
        };
        write!(f, "{}", s)
    }
}

/// Represents a CFG graph
#[derive(Debug)]
pub(crate) struct CfgGraph {
    cfg: Cfg,
    nodes: Vec<Rc<Node>>,
    edges: Vec<Edge>,
}

impl CfgGraph {
    fn new(cfg: Cfg) -> Self {
        Self {
            cfg,
            nodes: Vec::<Rc<Node>>::new(),
            edges: vec![],
        }
    }

    fn next_symbol(&self, s: &str) -> Option<String> {
        let i = s.find('.')
            .expect("No dot");
        let v: &str = s.get(i + 1..i + 2)?;

        Some(v.to_string())
    }

    /// Build edge for given non-terminal `nt`. Using `Y` as the non-terminal,
    ///  - `X: P 'q' . Y -> X: P 'q' Y.`
    ///  where rule `Y: 'r'`
    ///  Edges:
    ///  - [X: P q . Y] -->[Y] [X: P q Y .] -- shifting non-terminal `Y`
    ///  - [X: P q . Y]-->[<eps>] [Y: . r] -- derivation
    ///  - [Y: . r] -->[r] [Y: r .] -- shifting terminal `r`
    ///  - [Y: r .] -->[<eps>] [X; P q Y .] -- reduction
    fn build_edge(&self, nt: &str, parent: &Rc<Node>) -> Option<(Vec<Rc<Node>>, Vec<Edge>, Vec<Rc<Node>>)> {
        let mut nodes = Vec::<Rc<Node>>::new();
        let mut edges = Vec::<Edge>::new();
        // let mut derive_nodes = Vec::<Rc<Node>>::new();
        let mut reduce_nodes = Vec::<Rc<Node>>::new();
        let rule = self.cfg.get_rule(nt)?;
        for alt in &rule.rhs {
            println!("alt: {}", alt);
            for (i, sym) in alt.lex_symbols.iter().enumerate() {
                let src_sym_node = Rc::new(
                    Node::new(rule.lhs.to_owned(), alt.lex_symbols.clone(), i)
                );
                let tgt_sym_node = Rc::new(
                    Node::new(rule.lhs.to_owned(), alt.lex_symbols.clone(), i + 1)
                );
                nodes.push(Rc::clone(&src_sym_node));
                nodes.push(Rc::clone(&tgt_sym_node));

                // create the derive edge to the parent
                if i == 0 {
                    let d_edge = Edge::new(
                        Rc::clone(&parent),
                        Rc::clone(&src_sym_node),
                        LexSymbol::Epsilon(EpsilonSymbol::new()),
                        EdgeType::Derive);
                    edges.push(d_edge);
                }

                match sym {
                    LexSymbol::NonTerm(nt) => {
                        // if sym is non-terminal, invoke build_edge on it
                        // but before that check for *cycle*
                        let (
                            mut sym_nodes,
                            mut sym_edges,
                            r_nodes) = self.build_edge(
                            nt.tok.as_str(),
                            &src_sym_node,
                        )?;

                        nodes.append(&mut sym_nodes);
                        edges.append(&mut sym_edges);

                        // connect the r_nodes to tgt_sym_node
                        for r_n in r_nodes {
                            let r_edge = Edge::new(
                                Rc::clone(&r_n),
                                Rc::clone(&tgt_sym_node),
                                LexSymbol::Epsilon(EpsilonSymbol::new()),
                                EdgeType::Reduce,
                            );
                            edges.push(r_edge);
                        }
                    }
                    LexSymbol::Term(t) => {
                        let sym_edge = Edge::new(
                            Rc::clone(&src_sym_node),
                            Rc::clone(&tgt_sym_node),
                            sym.clone(),
                            EdgeType::Shift);
                        edges.push(sym_edge);
                    }
                    LexSymbol::Epsilon(eps) => {
                        //
                    }
                }

                if i == alt.lex_symbols.len() - 1 {
                    reduce_nodes.push(Rc::clone(&tgt_sym_node));
                }
            }
        }

        Some((nodes, edges, reduce_nodes))
    }

    /// Create the two root edges `[:.root]` and `[:root.]` and start building edges.
    pub(crate) fn start_edging(&self) -> (Vec<Rc<Node>>, Vec<Edge>) {
        let root_s = Rc::new(
            Node::new(
                "".to_owned(),
                vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
                0)
        );
        let root_e = Rc::new(
            Node::new(
                "".to_owned(),
                vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
                1)
        );
        let mut cfg_nodes: Vec<Rc<Node>> = vec![Rc::clone(&root_s), Rc::clone(&root_e)];
        let mut cfg_edges: Vec<Edge> = vec![];
        let (mut nodes, mut edges, mut r_nodes) = self.build_edge("root", &root_s)
            .expect("xxx");

        cfg_nodes.append(&mut nodes);
        cfg_edges.append(&mut edges);

        // connect the reduce nodes to root_e
        for r_n in r_nodes {
            let r_edge = Edge::new(
                Rc::clone(&r_n),
                Rc::clone(&root_e),
                LexSymbol::Epsilon(EpsilonSymbol::new()),
                EdgeType::Reduce,
            );
            cfg_edges.push(r_edge);
        }

        (cfg_nodes, cfg_edges)
    }
}

pub(crate) fn graph(cfgp: &str) -> Result<CfgGraph, CfgParseError> {
    let cfg = parse::parse(cfgp)?;
    println!("cfg:\n{}", cfg);
    let mut graph = CfgGraph::new(cfg);

    Ok(graph)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::cfg::{LexSymbol, NonTermSymbol};
    use crate::cfg::graph::{graph, Node};

    #[test]
    fn test_cfg_build_edges() {
        let mut g = graph("./grammars/simple.y")
            .expect("grammar parse failed");
        let (nodes, edges) = g.start_edging();
        println!("\n=> nodes:\n");
        for n in nodes {
            println!("n: {}", n);
        }
        println!("\n=> edges:\n");
        for e in edges {
            println!("e: {}", e);
        }
    }

    #[test]
    fn test_cfg_rec_build_edges() {
        let mut g = graph("./grammars/rec_direct.y")
            .expect("grammar parse failed");
        let (nodes, edges) = g.start_edging();
        println!("\n=> nodes:\n");
        for n in nodes {
            println!("n: {}", n);
        }
        println!("\n=> edges:\n");
        for e in edges {
            println!("e: {}", e);
        }
    }
}

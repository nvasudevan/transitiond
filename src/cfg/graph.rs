use std::{
    fmt,
    rc::Rc,
};

use crate::cfg::{Cfg, EpsilonSymbol, LexSymbol, NonTermSymbol, parse};
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
    // experimental
    pub(crate) node_id: usize
}

impl Node {
    /// index points to the index of the dot
    pub(crate) fn new(lhs: String, item: Vec<LexSymbol>, index: usize, node_id: usize) -> Self {
        Self {
            lhs,
            item,
            index,
            node_id
        }
    }

    pub(crate) fn item_string(&self) -> String {
        let pre = self.item.get(0..self.index)
            .expect("fail to retrieve items");
        let pre_s: Vec<String> = pre.iter()
            .map(|x| x.to_string()).collect();
        let post = self.item.get(self.index..)
            .expect("fail to retrieve items");
        let post_s: Vec<String> = post.iter()
            .map(|x| x.to_string()).collect();

        format!("[{}: {}.{}]", self.lhs, pre_s.join(" "), post_s.join(" "))
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

        write!(f, "{}", format!("[{}][{}: {}.{}]",
                                self.node_id,
                                self.lhs,
                                pre_s.join(" "),
                                post_s.join(" "))
        )
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        if self.item_string().eq(&other.item_string()) {
            return true;
        }

        false
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

    pub(crate) fn derive(source: Rc<Node>, target: Rc<Node>) -> Self {
        Self {
            source,
            target,
            derived: LexSymbol::Epsilon(EpsilonSymbol::new()),
            edge_type: EdgeType::Derive
        }
    }

    pub(crate) fn reduce(source: Rc<Node>, target: Rc<Node>) -> Self {
        Self {
            source,
            target,
            derived: LexSymbol::Epsilon(EpsilonSymbol::new()),
            edge_type: EdgeType::Reduce
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

    fn check_cycle(&self, parent: &Rc<Node>, node: &Rc<Node>) -> bool {
        println!("=> [cycle] :: parent: {}, node: {}", parent, node);
        if parent.eq(&node) {
            println!("** IS CYCLE **");
            return true;
        }

        false
    }

    /// Build edge for given non-terminal `nt`. Using `Y` as the non-terminal,
    ///  - `X: P 'q' . Y -> X: P 'q' Y.`
    ///  where rule `Y: 'r'`
    ///  Edges:
    ///  - [X: P q . Y] -->[Y] [X: P q Y .] -- shifting non-terminal `Y`
    ///  - [X: P q . Y]-->[<eps>] [Y: . r] -- derivation
    ///  - [Y: . r] -->[r] [Y: r .] -- shifting terminal `r`
    ///  - [Y: r .] -->[<eps>] [X; P q Y .] -- reduction
    fn build_edge(&self, nt: &str, parent: &Rc<Node>, mut node_id: usize) -> Option<(Vec<Rc<Node>>, Vec<Edge>, Vec<Rc<Node>>)> {
        let mut nodes = Vec::<Rc<Node>>::new();
        let mut edges = Vec::<Edge>::new();
        let mut reduce_nodes = Vec::<Rc<Node>>::new();

        let rule = self.cfg.get_rule(nt)?;
        for alt in &rule.rhs {
            let mut prev_node = Rc::clone(&parent);
            println!("alt: {}", alt);
            for (i, sym) in alt.lex_symbols.iter().enumerate() {
                let src_sym_node = match i {
                    0 => {
                        node_id += 1;
                        Rc::new(
                            Node::new(
                                rule.lhs.to_owned(),
                                alt.lex_symbols.clone(), i, node_id)
                        )
                    },
                    _ => { prev_node }
                };
                // create the derive edge to the parent
                if i == 0 {
                    nodes.push(Rc::clone(&src_sym_node));
                    edges.push(
                        Edge::derive(
                            Rc::clone(&parent), Rc::clone(&src_sym_node)
                        )
                    );
                }

                node_id += 1;
                let tgt_sym_node = Rc::new(
                    Node::new(rule.lhs.to_owned(),
                              alt.lex_symbols.clone(),
                              i + 1, node_id)
                );
                nodes.push(Rc::clone(&tgt_sym_node));

                match sym {
                    LexSymbol::NonTerm(nt) => {
                        // if sym is non-terminal, invoke build_edge on it
                        // but before that check for *cycle*
                        if self.check_cycle(parent, &src_sym_node) {
                             println!("find derive nodes from parent: {}", parent);
                        } else {
                            let (
                                mut sym_nodes,
                                mut sym_edges,
                                r_nodes) = self.build_edge(
                                nt.tok.as_str(),
                                &src_sym_node,
                                node_id
                            )?;

                            nodes.append(&mut sym_nodes);
                            edges.append(&mut sym_edges);

                            // connect the r_nodes to tgt_sym_node
                            for r_n in r_nodes {
                                edges.push(Edge::reduce(
                                    Rc::clone(&r_n),
                                    Rc::clone(&tgt_sym_node)
                                ));
                            }
                        }
                    }
                    LexSymbol::Term(_) => {
                        let sym_edge = Edge::new(
                            Rc::clone(&src_sym_node),
                            Rc::clone(&tgt_sym_node),
                            sym.clone(),
                            EdgeType::Shift);
                        edges.push(sym_edge);
                    }
                    LexSymbol::Epsilon(_) => {
                        //
                    }
                }

                if i == alt.lex_symbols.len() - 1 {
                    reduce_nodes.push(Rc::clone(&tgt_sym_node));
                }

                // now set the prev_node to tgt_sym_node for the symbols with i>0;
                prev_node = Rc::clone(&tgt_sym_node);
            }
        }

        Some((nodes, edges, reduce_nodes))
    }

    /// Create the two root edges `[:.root]` and `[:root.]` and start building edges.
    pub(crate) fn start_edging(&self) -> (Vec<Rc<Node>>, Vec<Edge>) {
        let mut node_id = 0;
        let root_s = Rc::new(
            Node::new(
                "".to_owned(),
                vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
                0,
                node_id)
        );
        let root_e = Rc::new(
            Node::new(
                "".to_owned(),
                vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
                1,
                node_id)
        );
        let mut cfg_nodes: Vec<Rc<Node>> = vec![Rc::clone(&root_s), Rc::clone(&root_e)];
        let mut cfg_edges: Vec<Edge> = vec![];
        let (
            mut nodes,
            mut edges,
            r_nodes
        ) = self.build_edge("root", &root_s, node_id).expect("xxx");

        cfg_nodes.append(&mut nodes);
        cfg_edges.append(&mut edges);

        // connect the reduce nodes to root_e
        for r_n in r_nodes {
            cfg_edges.push(Edge::reduce(
                Rc::clone(&r_n),
                Rc::clone(&root_e)
            ));
        }

        (cfg_nodes, cfg_edges)
    }
}

pub(crate) fn graph(cfgp: &str) -> Result<CfgGraph, CfgParseError> {
    let cfg = parse::parse(cfgp)?;
    println!("cfg:\n{}", cfg);
    let graph = CfgGraph::new(cfg);

    Ok(graph)
}

#[cfg(test)]
mod tests {
    use crate::cfg::graph::graph;

    #[test]
    fn test_cfg_build_edges() {
        let g = graph("./grammars/simple.y")
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
        let g = graph("./grammars/rec_direct.y")
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

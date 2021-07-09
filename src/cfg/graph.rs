use std::{
    fmt,
    rc::Rc,
};

use crate::cfg::{Cfg, EpsilonSymbol, LexSymbol, NonTermSymbol, parse};
use crate::cfg::parse::CfgParseError;
use std::collections::HashMap;

/// Represents a Cfg graph node/vertex
#[derive(Debug, Clone)]
pub(crate) struct Node {
    /// the non-terminal rule name
    pub(crate) lhs: String,
    /// the label of the node
    pub(crate) item: Vec<LexSymbol>,
    /// index of the symbol to be derived next
    pub(crate) index: usize,
    /// node id -- debug purposes
    pub(crate) node_id: usize,
}

impl Node {
    /// index points to the index of the dot
    pub(crate) fn new(lhs: String, item: Vec<LexSymbol>, index: usize, node_id: usize) -> Self {
        Self {
            lhs,
            item,
            index,
            node_id,
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
            edge_type: EdgeType::Derive,
        }
    }

    pub(crate) fn reduce(source: Rc<Node>, target: Rc<Node>) -> Self {
        Self {
            source,
            target,
            derived: LexSymbol::Epsilon(EpsilonSymbol::new()),
            edge_type: EdgeType::Reduce,
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

#[derive(Debug)]
pub(crate) struct InOutEdges {
    /// All the incoming edges of a node
    pub(crate) in_edges: Vec<Rc<Edge>>,
    /// All the outgoing edges of a node
    pub(crate) out_edges: Vec<Rc<Edge>>,
}

impl InOutEdges {
    pub(crate) fn new() -> Self {
        Self {
            in_edges: vec![],
            out_edges: vec![],
        }
    }

    pub(crate) fn add_in_edge(&mut self, edge: Rc<Edge>) {
        self.in_edges.push(edge);
    }

    pub(crate) fn add_out_edge(&mut self, edge: Rc<Edge>) {
        self.out_edges.push(edge);
    }
}

impl fmt::Display for InOutEdges {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::from("\n=> in");
        for in_edge in &self.in_edges {
            s.push_str(&format!("\n{}", in_edge.to_string()));
        }

        s.push_str("\nout =>");
        for out_edge in &self.out_edges {
            s.push_str(&format!("\n{}", out_edge.to_string()));
        }

        write!(f, "{}", s)
    }
}

pub(crate) struct GraphResult {
    pub(crate) nodes: Vec<Rc<Node>>,
    pub(crate) edges: Vec<Rc<Edge>>,
    pub(crate) node_edge_map: HashMap<usize, InOutEdges>,
    pub(crate) reduce_nodes: Vec<Rc<Node>>,
    node_id: usize,
}

impl GraphResult {
    pub(crate) fn new() -> Self {
        Self {
            nodes: vec![],
            edges: vec![],
            node_edge_map: Default::default(),
            reduce_nodes: vec![],
            node_id: 0,
        }
    }

    pub(crate) fn node_id(&self) -> usize {
        self.node_id
    }

    pub(crate) fn inc_node_id(&mut self) -> usize {
        self.node_id += 1;
        self.node_id
    }

    pub(crate) fn add_node(&mut self, node: Rc<Node>) {
        self.nodes.push(node);
    }

    pub(crate) fn add_edge(&mut self, edge: Rc<Edge>) {
        let src_node = &edge.source;
        let tgt_node = &edge.target;
        match self.node_edge_map.get_mut(&src_node.node_id) {
            Some(p_in_out) => {
                p_in_out.add_out_edge(Rc::clone(&edge));
            }
            _ => {
                let mut in_out = InOutEdges::new();
                in_out.add_out_edge(Rc::clone(&edge));
                self.node_edge_map.insert(src_node.node_id, in_out);
            }
        }

        match self.node_edge_map.get_mut(&tgt_node.node_id) {
            Some(p_in_out) => {
                p_in_out.add_in_edge(Rc::clone(&edge));
            }
            _ => {
                let mut in_out = InOutEdges::new();
                in_out.add_in_edge(Rc::clone(&edge));
                self.node_edge_map.insert(tgt_node.node_id, in_out);
            }
        }

        self.edges.push(edge);
    }

    pub(crate) fn update_reduce_edges(&mut self, target_node: &Rc<Node>) {
        loop {
            match self.reduce_nodes.pop() {
                Some(r_n) => {
                    self.add_edge(Rc::from(Edge::reduce(
                        Rc::clone(&r_n),
                        Rc::clone(&target_node),
                    )));
                }
                _ => { break }
            }
        }
    }
}

/// Represents a CFG graph
#[derive(Debug)]
pub(crate) struct CfgGraph {
    cfg: Cfg,
    nodes: Vec<Rc<Node>>,
    edges: Vec<Rc<Edge>>,
}

impl CfgGraph {
    fn new(cfg: Cfg) -> Self {
        Self {
            cfg,
            nodes: Vec::<Rc<Node>>::new(),
            edges: vec![],
        }
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
    fn build_edge(&self, nt: &str, parent: &Rc<Node>, mut g_result: &mut GraphResult) {
        let rule = self.cfg.get_rule(nt).expect("No such rule");
        for alt in &rule.rhs {
            let mut prev_node = Rc::clone(&parent);
            for (i, sym) in alt.lex_symbols.iter().enumerate() {
                let src_sym_node = match i {
                    0 => {
                        Rc::new(
                            Node::new(
                                rule.lhs.to_owned(),
                                alt.lex_symbols.clone(),
                                i,
                                g_result.inc_node_id()
                            )
                        )
                    }
                    _ => { prev_node }
                };
                // create the derive edge from the parent
                if i == 0 {
                    g_result.add_node(Rc::clone(&src_sym_node));
                    let derive_edge = Rc::from(Edge::derive(
                        Rc::clone(&parent), Rc::clone(&src_sym_node),
                    ));
                    g_result.add_edge(derive_edge);
                    // g_result.add_node_edge(derive_edge);
                }

                let tgt_sym_node = Rc::new(
                    Node::new(
                        rule.lhs.to_owned(),
                        alt.lex_symbols.clone(),
                        i + 1,
                        g_result.inc_node_id()
                    )
                );
                g_result.add_node(Rc::clone(&tgt_sym_node));

                match sym {
                    LexSymbol::NonTerm(nt) => {
                        // if sym is non-terminal, invoke build_edge on it
                        // but before that check for *cycle*
                        if self.check_cycle(parent, &src_sym_node) {
                            println!("find derive nodes from parent: {}", parent);
                        } else {
                            self.build_edge( nt.tok.as_str(), &src_sym_node, &mut g_result );
                            g_result.update_reduce_edges(&tgt_sym_node);
                        }
                    }
                    LexSymbol::Term(_) => {
                        let shift_edge = Edge::new(
                            Rc::clone(&src_sym_node),
                            Rc::clone(&tgt_sym_node),
                            sym.clone(),
                            EdgeType::Shift);
                        g_result.add_edge(Rc::from(shift_edge));
                    }
                    LexSymbol::Epsilon(_) => {
                        //
                    }
                }

                if i == alt.lex_symbols.len() - 1 {
                    g_result.reduce_nodes.push(Rc::clone(&tgt_sym_node));
                }

                // now set the prev_node to tgt_sym_node for the symbols with i>0;
                prev_node = Rc::clone(&tgt_sym_node);
            }
        }
    }

    /// Create the two root edges `[:.root]` and `[:root.]` and start building edges.
    pub(crate) fn start_edging(&self) -> GraphResult {
        let mut g_result = GraphResult::new();
        let root_s_node = Node::new(
            "".to_owned(),
            vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
            0,
            g_result.node_id());
        let root_s = Rc::new(root_s_node);

        let root_e_node = Node::new(
            "".to_owned(),
            vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))],
            1,
            g_result.inc_node_id());
        let root_e = Rc::new(root_e_node);
        g_result.add_node(Rc::clone(&root_s));
        g_result.add_node(Rc::clone(&root_e));

        // start build edges from root rule
        self.build_edge( "root", &root_s, &mut g_result);

        // connect the reduce nodes to root_e
        g_result.update_reduce_edges(&root_e);

        g_result
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
        let g_result = g.start_edging();
        println!("\n=> nodes:\n");
        for n in g_result.nodes {
            println!("n: {}", n);
        }
        println!("\n=> edges:\n");
        for e in g_result.edges {
            println!("e: {}", e);
        }
    }

    #[test]
    fn test_cfg_rec_build_edges() {
        let g = graph("./grammars/rec_direct.y")
            .expect("grammar parse failed");
        let g_result  = g.start_edging();
        println!("\n=> nodes:\n");
        for n in g_result.nodes {
            println!("n: {}", n);
        }
        println!("\n=> edges:\n");
        for e in g_result.edges {
            println!("e: {}", e);
        }
        for (node_id, in_out) in g_result.node_edge_map.iter() {
            println!("\n=> node: {}{}", node_id, in_out);
        }
    }
}

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
    /// Parent node that this node derived from
    /// For root node, this is set to None
    pub(crate) parent_node: Option<Rc<Node>>
}

impl Node {
    /// Create a new node with rule `lhs` and the alternative;
    /// index points to the dot location, and parent_node is the node
    /// (or non-terminal) from which this node eventually derived.
    pub(crate) fn new(lhs: &str,
                      item: &Vec<LexSymbol>,
                      index: usize,
                      node_id: usize,
                      parent_node: Option<Rc<Node>>
    ) -> Self {
        Self {
            lhs: lhs.to_owned(),
            item: item.clone(),
            index,
            node_id,
            parent_node
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

        let parent_node = match &self.parent_node {
            Some(p_n) => { p_n.node_id.to_string() },
            _ => { String::new() }
        };
        write!(f, "{}", format!("[{}->{}][{}: {}.{}]",
                                parent_node,
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

#[derive(Debug, PartialEq)]
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

pub(crate) struct CyclicLink {
    pub(crate) node: Rc<Node>,
    pub(crate) parent: Rc<Node>
}

impl CyclicLink {
    pub(crate) fn new(node: &Rc<Node>, parent: &Rc<Node>) -> Self {
        Self {
            node: Rc::clone(node),
            parent: Rc::clone(parent)
        }
    }
}

impl fmt::Display for CyclicLink {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = format!("[cycle] parent: {}, node: {}", self.parent, self.node);
        write!(f, "{}", s)
    }
}

pub(crate) struct GraphResult {
    pub(crate) nodes: Vec<Rc<Node>>,
    pub(crate) edges: Vec<Rc<Edge>>,
    pub(crate) node_edge_map: HashMap<usize, InOutEdges>,
    pub(crate) reduce_nodes: Vec<Rc<Node>>,
    pub(crate) cyclic_links: Vec<CyclicLink>,
    node_id: usize,
}

impl GraphResult {
    pub(crate) fn new() -> Self {
        Self {
            nodes: vec![],
            edges: vec![],
            node_edge_map: Default::default(),
            reduce_nodes: vec![],
            cyclic_links: vec![],
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

    pub(crate) fn add_cycle_link(&mut self, link: CyclicLink) {
        self.cyclic_links.push(link)
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

    /// Add a derivation edge (`derive` - deriving a non-terminal)
    fn add_derive_edge(&mut self, from: &Rc<Node>, node: &Rc<Node>) {
        let derive_edge = Rc::from(Edge::derive(
            Rc::clone(from), Rc::clone(node),
        ));
        self.add_edge(derive_edge);
    }

    /// Add a shift edge (`shift` - shifting of a terminal)
    fn add_shift_edge(&mut self, src: &Rc<Node>, tgt: &Rc<Node>, derived: &LexSymbol) {
        let shift_edge = Edge::new(
            Rc::clone(&src),
            Rc::clone(&tgt),
            derived.clone(),
            EdgeType::Shift);
        self.add_edge(Rc::from(shift_edge));
    }

    /// Update reduce edges to `target_node`.
    pub(crate) fn update_reductions(&mut self, target: &Rc<Node>) {
        let edges: Vec<Rc<Edge>> = self.reduce_nodes
            .drain(..)
            .map({
                |src|
                    Rc::from(
                        Edge::reduce(Rc::clone(&src), Rc::clone(&target))
                    )
            })
            .collect();

        for edge in edges {
            self.add_edge(edge);
        }
    }

    /// Add cycle derivations from `cyclic_links`
    fn add_cycle_derivations(&mut self) {
        let mut cycle_edges = Vec::<Rc<Edge>>::new();
        for link in &self.cyclic_links {
            let edges = self.node_edge_map.get(&link.parent.node_id)
                .expect(&format!("Unable to find parent: {}", &link.parent));
            let nodes: Vec<Rc<Node>> = edges.out_edges
                .iter()
                .filter(|e| e.edge_type == EdgeType::Derive)
                .map(|e| Rc::clone(&e.target))
                .collect();

            for drv_node in nodes {
                let derive_edge = Rc::from(Edge::derive(
                    Rc::clone(&link.node), Rc::clone(&drv_node),
                ));
                cycle_edges.push(derive_edge);
            }
        }

        // now add the edges
        for e in cycle_edges {
            self.add_edge(e);
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

    /// Checks for direct cycle (`A: A`) or indirect cycle (`A: B; B: A`).
    /// Keep traversing up the tree to find a matching ancestor node
    /// On reaching root node, return None
    fn check_cycle<'a>(&self, node: &'a Rc<Node>) -> Option<&'a Rc<Node>> {
        let mut curr_ancestor = &node.parent_node;
        loop {
            match curr_ancestor {
                Some(ancestor) => {
                    if ancestor.eq(&node) {
                        println!("=> [CYCLE]:: parent: {} <- node: {}", ancestor, node);
                        return Some(ancestor);
                    }
                    curr_ancestor = &ancestor.parent_node;
                }
                _ => { break }
            }
        }

        None
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
        let mut reduce_nodes: Vec<Rc<Node>> = vec![];
        for alt in &rule.rhs {
            let mut prev_node = Rc::clone(&parent);
            for (i, sym) in alt.lex_symbols.iter().enumerate() {
                let src_sym_node = match i {
                    0 => {
                        let src = Rc::new(
                            Node::new(
                                &rule.lhs,
                                &alt.lex_symbols,
                                i,
                                g_result.inc_node_id(),
                                Some(Rc::clone(parent))
                            )
                        );
                        // create the derive edge from the parent
                        g_result.add_node(Rc::clone(&src));
                        g_result.add_derive_edge(&parent, &src);
                        src
                    }
                    _ => { prev_node }
                };

                match sym {
                    LexSymbol::Term(_) | LexSymbol::Epsilon(_) => {
                        let tgt_sym_node = Rc::new(
                            Node::new(
                                &rule.lhs,
                                &alt.lex_symbols,
                                i+1,
                                g_result.inc_node_id(),
                                Option::from(Rc::clone(parent))
                            )
                        );
                        g_result.add_node(Rc::clone(&tgt_sym_node));
                        g_result.add_shift_edge(&src_sym_node, &tgt_sym_node, &sym);

                        if i == alt.lex_symbols.len() - 1 {
                            reduce_nodes.push(Rc::clone(&tgt_sym_node));
                        }

                        // now set the prev_node to tgt_sym_node for the symbols with i>0;
                        prev_node = Rc::clone(&tgt_sym_node);
                    }
                    LexSymbol::NonTerm(nt) => {
                        // if sym is non-terminal, invoke build_edge on it
                        // first, check for *cycle*
                        match self.check_cycle(&src_sym_node) {
                            Some(parent_cycle_node) => {
                                let link = CyclicLink::new(&src_sym_node, parent_cycle_node);
                                g_result.add_cycle_link(link);
                                // g_result.add_cycle_derivations(parent_cycle_node, &src_sym_node);

                                // exit this iteration for this alternative
                                // we don't set prev_node as we are exiting this iteration
                                // we also don't have reduce nodes, as they will be handled
                                // by the tree from the drv_nodes.
                                break;
                            }
                            _ => {
                                self.build_edge(nt.tok.as_str(), &src_sym_node, &mut g_result);
                                let tgt_sym_node = Rc::new(
                                    Node::new(
                                        &rule.lhs,
                                        &alt.lex_symbols,
                                        i+1,
                                        g_result.inc_node_id(),
                                        Option::from(Rc::clone(parent))
                                    )
                                );
                                g_result.add_node(Rc::clone(&tgt_sym_node));
                                g_result.update_reductions(&tgt_sym_node);

                                if i == alt.lex_symbols.len() - 1 {
                                    reduce_nodes.push(Rc::clone(&tgt_sym_node));
                                }

                                // now set prev_node to tgt_sym_node for symbols with index > 0
                                prev_node = Rc::clone(&tgt_sym_node);
                            }
                        }
                    }
                }
            }
        }

        // now we are ready to append the reduce nodes
        g_result.reduce_nodes.append(&mut reduce_nodes);
    }

    /// Create the two root edges `[:.root]` and `[:root.]` and start building edges.
    pub(crate) fn start_edging(&self) -> GraphResult {
        let mut g_result = GraphResult::new();
        let root_item = vec![LexSymbol::NonTerm(NonTermSymbol::new("root".to_owned()))];
        let root_s_node = Node::new(
            "", &root_item, 0, g_result.node_id(), None
        );
        let root_s = Rc::new(root_s_node);

        let root_e_node = Node::new(
            "", &root_item, 1, g_result.inc_node_id(), None
        );
        let root_e = Rc::new(root_e_node);
        g_result.add_node(Rc::clone(&root_s));
        g_result.add_node(Rc::clone(&root_e));

        // start build edges from root rule
        self.build_edge("root", &root_s, &mut g_result);

        // set the cycle links
        g_result.add_cycle_derivations();

        // connect the reduce nodes to root_e
        g_result.update_reductions(&root_e);

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
        for (node_id, in_out) in g_result.node_edge_map.iter() {
            println!("\n=> node: {}{}", node_id, in_out);
        }
    }

    #[test]
    fn test_cfg_rec_build_edges() {
        let g = graph("./grammars/rec_direct.y")
            .expect("grammar parse failed");
        let g_result = g.start_edging();
        println!("\n=> nodes:\n");
        for n in g_result.nodes {
            println!("{}", n);
        }
        println!("\n=> edges:\n");
        for e in g_result.edges {
            println!("{}", e);
        }
        for (node_id, in_out) in g_result.node_edge_map.iter() {
            println!("\n=> node: {}{}", node_id, in_out);
        }
    }

    #[test]
    fn test_cfg_indirect_rec_build_edges() {
        let g = graph("./grammars/rec_indirect.y")
            .expect("grammar parse failed");
        let g_result = g.start_edging();
        println!("\n=> nodes:\n");
        for n in g_result.nodes {
            println!("{}", n);
        }
        println!("\n=> edges:\n");
        for e in g_result.edges {
            println!("{}", e);
        }
        for (node_id, in_out) in g_result.node_edge_map.iter() {
            println!("\n=> node: {}{}", node_id, in_out);
        }
    }

    #[test]
    fn test_cfg_build() {
        let g = graph("./grammars/medium.y")
            .expect("grammar parse failed");
        let g_result = g.start_edging();
        println!("\n=> nodes:\n");
        for n in g_result.nodes {
            println!("{}", n);
        }
        println!("\n=> edges:\n");
        for e in g_result.edges {
            println!("{}", e);
        }
        for (node_id, in_out) in g_result.node_edge_map.iter() {
            println!("\n=> node: {}{}", node_id, in_out);
        }
    }
}

use crate::cfg::{TermSymbol, LexSymbol, Cfg, CfgRule, RuleAlt};

const EPSILON: &str = "epsilon";

/// Represents a CFG graph
#[derive(Debug)]
pub(crate) struct CfgGraph {
    // cfg: Cfg,
    vertices: Vec<Vertex>,
    edges: Vec<Edge>
}

/// Represents a Cfg graph vertex
#[derive(Debug, Clone)]
pub(crate) struct Vertex {
    label: String
}

impl Vertex {
    pub(crate) fn new(label: String) -> Self {
        Self {
            label
        }
    }
}

impl PartialEq for Vertex {
    fn eq(&self, other: &Self) -> bool {
        self.label.eq(&other.label)
    }
}
/// An `Edge` represents a derivation where a set of symbols have been consumed.
#[derive(Debug)]
pub(crate) struct Edge {
    /// start vertext of the edge
    source: Vertex,
    /// end vertext of the edge
    target: Vertex,
    /// terminals consumed
    derived: Vec<LexSymbol>,
    /// symbols yet to be derived
    to_be_derived: Vec<LexSymbol>
}

impl Edge {
    pub(crate) fn new(source: Vertex,
                      target: Vertex,
                      terminals: Vec<LexSymbol>,
                      to_be_derived: Vec<LexSymbol>) -> Self {

        Self {
            source,
            target,
            derived: terminals,
            to_be_derived
        }
    }
}

impl CfgGraph {
    pub(crate) fn new() -> Self {
        Self {
            // cfg,
            vertices: vec![],
            edges: vec![]
        }
    }

    pub(crate) fn add_vertex(&mut self, vertex: Vertex) {
        self.vertices.push(vertex);
    }

    pub(crate) fn set_vertices(&mut self, vertices: Vec<Vertex>) {
        self.vertices = vertices;
    }

    pub(crate) fn vertices(&self) -> &[Vertex] {
        self.vertices.as_slice()
    }

    fn find_vertex_by_label(&self, label: &str) -> Option<&Vertex> {
        for v in &self.vertices {
            if v.label.eq(label) {
                return Some(v);
            }
        }

        None
    }

    pub(crate) fn edges(&self) -> &[Edge] {
        self.edges.as_slice()
    }

    pub(crate) fn add_edge(&mut self, edge: Edge) {
        self.edges.push(edge);
    }

    pub(crate) fn set_edges(&mut self, edges: Vec<Edge>) {
        self.edges = edges;
    }

}

impl From<Cfg> for CfgGraph {
    fn from(cfg: Cfg) -> Self {
        let mut graph = CfgGraph::new();
        let mut vertices: Vec<Vertex> = cfg.rules
            .iter()
            .map(|r| Vertex::new(format!("u_{}", r.lhs)))
            .collect();

        let mut v_vertices: Vec<Vertex> = cfg.rules
            .iter()
            .map(|r| Vertex::new(format!("v_{}", r.lhs)))
            .collect();

        vertices.append(&mut v_vertices);
        graph.set_vertices(vertices);

        let alts = cfg.terminals_only_alts();
        let mut edges = Vec::<Edge>::new();
        for (lhs, alt) in alts {
            println!(" {}: {}", lhs, alt);
            let start_label = format!("u_{}", lhs);
            let end_label = format!("v_{}", lhs);
            let start_v = graph.find_vertex_by_label(&start_label)
                .expect(&format!("Unable to find vertex for label {}", start_label));
            let end_v = graph.find_vertex_by_label(&end_label)
                .expect(&format!("Unable to find vertex for label {}", end_label));
            let derived = &alt.lex_symbols;
            let to_be_derived: Vec<LexSymbol> = vec![LexSymbol::Term(TermSymbol::new(EPSILON.to_owned()))];
            edges.push(
                Edge::new(start_v.clone(), end_v.clone(), derived.clone(), to_be_derived)
            );
        }

        graph.set_edges(edges);

        graph
    }
}
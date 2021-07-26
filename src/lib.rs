use crate::cfg::diagram::TransitionGraph;

mod cfg;

pub fn transition_graph(cfgp: &str) -> TransitionGraph {
    let g = cfg::diagram::graph(cfgp)
        .expect(&format!("Unable to generate transition graph for grammar: {}", cfgp));

    g
}
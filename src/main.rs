use transitiond::transition_graph;

fn main() {
    println!("=> Building transition diagram ...");
    let g = transition_graph("./grammars/lr1.y");
}
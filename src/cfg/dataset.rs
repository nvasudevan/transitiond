use crate::cfg::{Cfg, mutate};
use crate::cfg::graph::{GraphResult, CfgGraph};
use std::path::Path;
use std::fmt;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CfgDataSetError {
    msg: String,
}

impl CfgDataSetError {
    pub(crate) fn new(msg: String) -> Self {
        CfgDataSetError {
            msg
        }
    }
}

impl fmt::Display for CfgDataSetError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

/// Represents the ML data associated with a Cfg Graph
pub(crate) struct CfgData {
    /// grammar file index
    cfg_id: usize,
    /// Graph associated with the grammar
    graph: GraphResult,
    /// label: 0 indicates grammar is unambiguous; 1 is ambiguous
    label: usize
}

impl CfgData {
    pub(crate) fn new(cfg_id: usize, graph: GraphResult) -> Self {
        Self {
            cfg_id,
            graph,
            label: 0
        }
    }
}

impl fmt::Display for CfgData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = format!(
            "[{}] graph: {}, label: {}", self.cfg_id, self.graph, self.label
        );
        write!(f, "{}", s)
    }
}

pub(crate) struct CfgDataSet {
    pub(crate) cfg_data: Vec<CfgData>,
    pub(crate) node_ids_map: HashMap<String, usize>,
    pub(crate) edge_ids_map: HashMap<String, usize>,
}

impl CfgDataSet {
    pub(crate) fn new(cfg_data: Vec<CfgData>) -> Self {
        Self {
            cfg_data,
            node_ids_map: Default::default(),
            edge_ids_map: Default::default()
        }
    }

    pub(crate) fn build_unique_nodes_map(&mut self) {
        println!("=> building the list of unique nodes ...");
        let mut node_ids_map: HashMap<String, usize> = HashMap::new();
        let mut node_id_counter: usize = 0;

        for cfg in &self.cfg_data {
            for node in &cfg.graph.nodes {
                let node_s = node.min_item_string();
                if ! node_ids_map.contains_key(&node_s) {
                    node_ids_map.insert(node_s, node_id_counter);
                    node_id_counter += 1;
                }
            }
        }
        self.node_ids_map = node_ids_map;
    }

    fn build_unique_edges_map(&mut self) {
        println!("=> building the list of unique edges ...");
        let mut edge_ids_map: HashMap<String, usize> = HashMap::new();
        let mut edge_id_counter: usize = 0;

        for cfg in &self.cfg_data {
            for e in &cfg.graph.edges {
                let e_s = e.edge_label();
                if ! edge_ids_map.contains_key(&e_s) {
                    edge_ids_map.insert(e_s, edge_id_counter);
                    edge_id_counter += 1;
                }
            }
        }
        self.edge_ids_map = edge_ids_map;

    }

    /// CFG_node_labels.txt - `i`th line indicates the node label of the `i`th node
    fn write_node_labels(&self, node_labels_file: &Path) -> Result<(), CfgDataSetError> {
        println!("=> writing node labels to {}", node_labels_file.to_str().unwrap());
        let mut node_labels: Vec<String> = vec![];
        for cfg in &self.cfg_data {
            for n in &cfg.graph.nodes {
                let n_label = n.min_item_string();
                let v = self.node_ids_map.get(&n_label)
                    .ok_or_else(||
                        CfgDataSetError::new(
                            format!("Didn't find node {} in node_ids_map", n))
                    )?;
                // println!("{} -- {}", n, v);
                node_labels.push(v.to_string());
            }
        }

        let node_labels_s = node_labels.join("\n");
        std::fs::write(node_labels_file, node_labels_s)
            .expect("Unable to write node ids' to file");

        Ok(())
    }

    /// Write the class labels for graph
    fn write_graph_labels(&self, graph_labels_file: &Path) -> Result<(), CfgDataSetError> {
        println!("=> writing graph labels to {}", graph_labels_file.to_str().unwrap());
        let graph_labels: Vec<String> = self.cfg_data
            .iter()
            .map(|c| c.label.to_string() )
            .collect();

        let graph_labels_s = graph_labels.join("\n");
        let _ = std::fs::write(graph_labels_file, graph_labels_s)
            .map_err(|e|  CfgDataSetError::new(e.to_string()));

        Ok(())
    }

    fn write_graph_indicators(&self, graph_indicator_file: &Path) -> Result<(), CfgDataSetError> {
        println!("=> writing graph indicators to {}", graph_indicator_file.to_str().unwrap());
        let mut graph_indicator: Vec<String> = vec![];
        for (i, cfg) in self.cfg_data.iter().enumerate() {
            for _ in &cfg.graph.nodes {
                graph_indicator.push((i+1).to_string());
            }
        }

        let graphs_indicator_s = graph_indicator.join("\n");
        let _ = std::fs::write(graph_indicator_file, graphs_indicator_s)
            .map_err(|e|  CfgDataSetError::new(e.to_string()));

        Ok(())
    }

    /// Create `CFG_A.txt` containing the sparse matrix of all edges
    fn write_edge_labels(&self, edge_labels_file: &Path, edges_file: &Path) -> Result<(), CfgDataSetError> {
        let mut n_i = 1;
        let mut edge_labels: Vec<String> = vec![];
        let mut edges: Vec<String> = vec![];
        for cfg in &self.cfg_data {
            println!("\n=> total: {}", cfg.graph.edges.len());
            for e in &cfg.graph.edges {
                let edge_label_code = self.edge_ids_map.get(&e.edge_label())
                    .ok_or_else(|| CfgDataSetError::new(
                        format!("Unable to retrieve code for edge: {}", e)
                    ))?;
                // edge_labels.push(format!("{}, {}", e.edge_label(), edge_label_code.to_string()));
                edge_labels.push(edge_label_code.to_string());
                let src_node = e.source_node_id() + n_i;
                let tgt_node = e.target_node_id() + n_i;
                // println!("[{} -> {}] - {}", src_node, tgt_node, e);
                edges.push(format!("{}, {}" , src_node, tgt_node));
            }
            n_i += cfg.graph.nodes.len();
        }

        let edge_labels_s = edge_labels.join("\n");
        let _ = std::fs::write(edge_labels_file, edge_labels_s)
            .map_err(|e|  CfgDataSetError::new(e.to_string()));

        let edges_s = edges.join("\n");
        let _ = std::fs::write(edges_file, edges_s)
            .map_err(|e|  CfgDataSetError::new(e.to_string()));

        Ok(())
    }

    /// n - total no of nodes
    /// m - total no of edges
    /// write:
    /// - node labels (CFG_node_labels.txt)
    /// - graph labels (CFG_graph_labels.txt)
    /// - node to graph mapping (CFG_graph_indicator.txt)
    /// - edge labels (CFG_edge_labels.txt)
    fn persist(&self, data_dir: &Path) -> Result<Vec<String>, CfgDataSetError> {
        let mut ds_files: Vec<String> = vec![];
        let node_labels_file = data_dir.join("CFG_node_labels.txt");
        self.write_node_labels(&node_labels_file)?;
        ds_files.push(node_labels_file.to_str().unwrap().to_owned());

        let graph_labels_file = data_dir.join("CFG_graph_labels.txt");
        self.write_graph_labels(&graph_labels_file)?;
        ds_files.push(graph_labels_file.to_str().unwrap().to_owned());

        let graph_indicator_file = data_dir.join("CFG_graph_indicator.txt");
        self.write_graph_indicators(&graph_indicator_file)?;
        ds_files.push(graph_indicator_file.to_str().unwrap().to_owned());

        let edge_labels_file = data_dir.join("CFG_edge_labels.txt");
        let edges_file = data_dir.join("CFG_A.txt");
        self.write_edge_labels(&edge_labels_file, &edges_file)?;
        ds_files.push(edge_labels_file.to_str().unwrap().to_owned());
        ds_files.push(edges_file.to_str().unwrap().to_owned());

        let readme_file = data_dir.join("README.txt");
        let _ = std::fs::write(&readme_file, "README");
        ds_files.push(readme_file.to_str().unwrap().to_owned());

        Ok(ds_files)
    }
}

pub(crate) fn generate(cfg: &Cfg) -> Vec<Cfg> {
    let mut cfg_mut = mutate::CfgMutation::new(&cfg);
    cfg_mut.instantiate();

    let cnt = cfg_mut.mut_cnt() * (cfg_mut.terms.len() - 1);
    let cfgs = mutate::run(&mut cfg_mut, cnt)
        .expect("Unable to generate a mutated cfg");
    println!("\n=> generated {} cfgs, creating dataset ...", cfgs.len());

    cfgs
}

pub(crate) fn build_dataset(cfgs: &Vec<Cfg>, data_dir: &Path) -> Result<Vec<String>, CfgDataSetError> {
    let mut cfg_data: Vec<CfgData> = vec![];
    for (i, cfg) in cfgs.iter().enumerate() {
        let cfgp = data_dir.join(i.to_string());
        std::fs::write(&cfgp, cfg.as_yacc())
            .map_err(|e| CfgDataSetError::new(
                format!("Error occurred whilst writing cfg.\n\nError: {}",
                        e.to_string())
            ))?;
        let g = CfgGraph::new(cfg.clone());
        let g_result = g.instantiate()
            .expect("Unable to convert cfg to graph");

        cfg_data.push(CfgData::new(i, g_result));
    }

    let mut ds = CfgDataSet::new(cfg_data);
    ds.build_unique_nodes_map();
    ds.build_unique_edges_map();
    println!("unique edges: {:?}", ds.edge_ids_map);
    let ds_files = ds.persist(&data_dir)?;

    Ok(ds_files)
}

#[cfg(test)]
mod tests {
    use crate::cfg::parse;
    use crate::cfg::dataset::{generate, build_dataset};
    use std::path::Path;

    #[test]
    fn test_ds_generate() {
        let cfg = parse::parse("./grammars/lr1.y")
            .expect("Unable to parse as a cfg");
        let cfgs = generate(&cfg);
        let data_dir = Path::new("/var/tmp/cfg_ds");
        let _ = build_dataset(&cfgs, &data_dir)
            .expect("Unable to build dataset from cfgs");
    }
}
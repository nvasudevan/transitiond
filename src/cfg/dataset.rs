use crate::cfg::{Cfg, mutate};
use crate::cfg::graph::{graph, GraphResult};
use std::path::Path;
use std::fmt;
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct CfgDataSetError {
    msg: String,
}

impl CfgDataSetError {
    fn new(msg: String) -> Self {
        CfgDataSetError {
            msg
        }
    }
}

/// Represents the ML data associated with a Cfg Graph
pub(crate) struct CfgData {
    /// grammar
    cfg: Cfg,
    /// Graph associated with the grammar
    graph: GraphResult,
    /// label: 0 indicates grammar is unambiguous; 1 is ambiguous
    label: bool
}

impl CfgData {
    pub(crate) fn new(cfg: Cfg, graph: GraphResult) -> Self {
        Self {
            cfg,
            graph,
            label: false
        }
    }
}

impl PartialEq for CfgData {
    fn eq(&self, other: &Self) -> bool {
        if self.cfg.eq(&other.cfg) {
            return true;
        }
        false
    }
}

impl fmt::Display for CfgData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = format!("graph: {}, label: {}", self.graph, self.label);
        write!(f, "{}", s)
    }
}

pub(crate) struct CfgDataSet {
    pub(crate) cfg_data: Vec<CfgData>,
    pub(crate) node_ids_map: HashMap<String, usize>,
}

impl CfgDataSet {
    pub(crate) fn new(cfg_data: Vec<CfgData>, node_ids_map: HashMap<String, usize>) -> Self {
        Self {
            cfg_data,
            node_ids_map,
        }
    }

    /// n - total no of nodes
    /// m - total no of edges
    /// CFG_node_labels.txt - `i`th line indicates the node label of the `i`th node
    fn persist(&self, data_dir: &Path) -> Result<(), CfgDataSetError> {
        println!("total unique nodes: {}", &self.node_ids_map.keys().len());
        let mut node_labels: Vec<String> = vec![];
        for cfg in &self.cfg_data {
            println!("\n\n==> cfg:\n{}", cfg.cfg);
            for n in &cfg.graph.nodes {
                let n_label = n.min_item_string();
                let v = self.node_ids_map.get(&n_label)
                    .ok_or_else(||
                        CfgDataSetError::new(
                            format!("Didn't find node {} in node_ids_map", n))
                    )?;
                println!("{} -- {}", n, v);
                node_labels.push(v.to_string());
            }
        }

        let node_ids_file = data_dir.join("node_ids.txt");
        let node_labels_s = node_labels.join("\n");
        std::fs::write(node_ids_file, node_labels_s)
            .expect("Unable to write node ids' to file");

        Ok(())
    }
}

pub(crate) fn generate(cfg: &Cfg, ds_dir: &str) -> CfgDataSet {
    let mut cfg_data: Vec<CfgData> = vec![];
    let mut cfg_mut = mutate::CfgMutation::new(&cfg);
    cfg_mut.instantiate();
    let cnt = cfg_mut.mut_cnt() * (cfg_mut.terms.len() - 1);
    let cfgs = mutate::run(&mut cfg_mut, cnt)
        .expect("Unable to generate a mutated cfg");
    println!("\n=> generated {} cfgs, creating dataset ...", cfgs.len());
    let mut node_ids_map: HashMap<String, usize> = HashMap::new();

    let mut node_id_counter: usize = 0;
    let ds_path = Path::new(ds_dir);
    for (i, cfg) in cfgs.iter().enumerate() {
        let cfgp = ds_path.join(i.to_string());
        std::fs::write(&cfgp, cfg.as_yacc())
            .expect(&format!("Failed to write cfg {}", cfg));
        let g = graph(cfgp.to_str().unwrap())
            .expect("Unable to create graph");
        let g_result = g.instantiate()
            .expect("Unable to convert cfg to graph");

        for node in &g_result.nodes {
            let node_s = node.min_item_string();
            if ! node_ids_map.contains_key(&node_s) {
                node_ids_map.insert(node_s, node_id_counter);
                node_id_counter += 1;
            }
        }

        cfg_data.push(CfgData::new(cfg.clone(), g_result));
    }

    let cfg_ds = CfgDataSet::new(cfg_data, node_ids_map);

    cfg_ds
}

#[cfg(test)]
mod tests {
    use crate::cfg::parse;
    use crate::cfg::dataset::generate;
    use std::path::Path;

    #[test]
    fn test_ds_generate() {
        let cfg = parse::parse("./grammars/lr1.y")
            .expect("Unable to parse as a cfg");
        let ds = generate(&cfg, "/var/tmp/cfg_ds");
        // for cfgd in &ds.cfg_data {
        //     println!("=> graph: {}, label: {}", cfgd.graph, cfgd.label);
        // }

        let data_dir = Path::new("/var/tmp/cfg_ds");
        ds.persist(data_dir)
            .expect("Unable to persist dataset");

    }
}
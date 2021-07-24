use transitiond::cfg_dataset_as_zip;
use std::path::Path;

fn main() {
    println!("=> Building cfg dataset ...");
    let gp = "./grammars/lr1.y";
    let ds_dir = "/var/tmp/cfg_ds";
    let data_dir = Path::new(ds_dir);
    let cfg_prefix = "CFG";
    match cfg_dataset_as_zip(&gp, &data_dir, cfg_prefix) {
        Ok(_) => { println!("cfg dataset built!") }
        Err(e) => {
            eprintln!("error: {}", e);
        }
    }
}
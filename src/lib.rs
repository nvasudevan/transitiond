use crate::cfg::parse;
use crate::cfg::dataset::{generate, build_dataset, CfgDataSetError};
use std::path::Path;
use std::fs;
use zip::write::FileOptions;
use std::io::Write;

mod cfg;
mod py3;

pub fn cfg_dataset(gf: &str, data_dir: &Path) -> Result<Vec<String>, CfgDataSetError> {
    let cfg = parse::parse(gf)
        .map_err( |e| CfgDataSetError::new(e.msg))?;
    let cfgs = generate(&cfg);
    let ds_files = build_dataset(&cfgs, &data_dir)?;

    Ok(ds_files)
}

pub fn cfg_dataset_as_zip(base_cfg: &str, data_dir: &Path, cfg_prefix: &str) -> Result<(), CfgDataSetError> {
    let ds_files = cfg_dataset(&base_cfg, &data_dir)?;
    let cfg_zip = data_dir.join(format!("{}.zip", cfg_prefix));
    let zip_p = fs::File::create(cfg_zip)
        .map_err(|e| CfgDataSetError::new(e.to_string()))?;
    let mut zip = zip::ZipWriter::new(zip_p);
    let zip_subdir = format!("{}/", cfg_prefix);
    let _ = zip.add_directory(&zip_subdir, Default::default())
        .map_err(|_| CfgDataSetError::new(
            format!("Unable to add '{}' directory to zip file", &zip_subdir)
        ));

    // add files to zip
    for ds_f in ds_files {
        let zip_file_options = FileOptions::default()
            .compression_method(zip::CompressionMethod::Stored)
            .unix_permissions(0o755);
        let ds_p = Path::new(&ds_f);
        let contents = std::fs::read_to_string(ds_p).unwrap();
        let zip_ds_p = format!("{}{}", zip_subdir, ds_p.file_name().unwrap().to_str().unwrap());
        let _ = zip.start_file(zip_ds_p, zip_file_options)
            .map_err(|_|CfgDataSetError::new(
                format!("Unable to create file within zip")
            ));
        zip.write_all(contents.as_bytes()).unwrap();
    }

    zip.finish().unwrap();

    Ok(())
}

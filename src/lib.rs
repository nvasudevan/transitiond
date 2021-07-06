mod cfg;
mod py3;

#[cfg(test)]
mod tests {
    use pyo3::prelude::*;
    use pyo3::types::IntoPyDict;
    use std::collections::HashMap;

    fn py3(py: Python) -> PyResult<()> {
        let sys = py.import("sys")?;
        // retrieve sys.version
        let ver: String = sys.get("version")?.extract()?;
        println!("ver: {}", ver);

        // retrieve sys.path, it's a list, so extract as a Vec
        let py_path: Vec<&str> = sys.get("path")?.extract::<Vec<&str>>()?;
        println!("path: {:?}", py_path);

        // extract env values as a HashMap
        let os = py.import("os")?;
        let envs: HashMap<&str, &str> = os.get("environ")?.extract::<HashMap<&str, &str>>()?;
        println!("envs: {:?}", envs);

        // run code
        let locals = [("os", py.import("os")?)].into_py_dict(py);
        let code = "os.getenv('USER') or os.getenv('USERNAME') or 'Unknown'";
        let user: String = py.eval(code, None, Some(locals))?.extract()?;
        println!("user: {}", user);

        Ok(())
    }

    #[test]
    fn py3_works() {
        let _ = Python::with_gil(|py| {
            py3(py).map_err(|e| {
                e.print_and_set_sys_last_vars(py)
            })
        });
    }
}

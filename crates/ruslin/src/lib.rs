extern crate nalgebra;

pub use nalgebra::base::dimension::Dynamic;
pub use nalgebra::base::DMatrix;
pub use nalgebra::linalg::SVD;


fn v2m(m: Vec<Vec<f64>>) -> DMatrix<f64> {
    let nrows=m.len();
    let ncols=m[0].len();
    let numitems=nrows*ncols;

    DMatrix::from_row_slice(nrows, ncols, &m.concat()[..numitems])
}

fn m2v(d: DMatrix<f64>) -> Vec<Vec<f64>> {
    let ncols=d.ncols();
    let mut ret = Vec::new();
    for r in d.transpose().as_slice().chunks(ncols) {
        ret.push(Vec::from(r))
    }
    ret
}


#[rustler::nif]
fn version() -> String {
    "Version 0.1".to_string()
}

#[rustler::nif]
fn transpose(m: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
    m2v(v2m(m).transpose())
}

#[rustler::nif]
fn matmul(a: Vec<Vec<f64>>, b: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
    let a = v2m(a);
    let b = v2m(b);
    match a.nrows() == b.ncols() || b.nrows() == a.ncols() {
        true => m2v(a * b),
        false => vec![],
    }
}

#[rustler::nif]
fn inv(m: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
    match v2m(m).try_inverse()
    {
        Some(m) => m2v(m),
        None => vec![],
    }
}

//#[rustler::nif]
//fn svd(m: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
//    match v2m(m).try_svd(true, true, 0.000001, 20)
//    {
//        Some(m) => m2v(m),
//        None => vec![],
//    }
//}

//fn svd(m: DMatrix<f64>) -> Result<SVD<f64, Dynamic, Dynamic>, Error> {
//    match m.try_svd(true, true, 0.000001, 20) {
//        Some(im) => Ok(im),
//        None => make_error("no convergence"),
//    }
//}


#[rustler::nif]
fn sum(xs: Vec<f64>) -> f64 {
    xs.iter().fold(0.0,|a, &b| a + b)
}

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!("linalg_ruslin", [version,add,sum,transpose,matmul,inv]);

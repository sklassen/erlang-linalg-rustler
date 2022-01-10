extern crate rustler;
use rustler::{Encoder, Env,Term,NifResult};
//use rustler::{NifEnv, NifTerm, NifError, NifDecoder, NifEncoder, NifResult};

extern crate nalgebra;
use nalgebra::base::DMatrix;
use nalgebra::linalg::SVD;


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
fn version() -> Vec<u8> {
    let version="Version 0.1";
    return version.chars().map(|x| x as u8).collect();
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

#[rustler::nif]
fn svd(m: Vec<Vec<f64>>) -> (Vec<Vec<f64>>,Vec<f64>,Vec<Vec<f64>>) {
    match v2m(m).try_svd(true, true, 0.000001, 20)
    {
        Some(SVD{u:Some(u),singular_values:s,v_t:Some(v_t)}) => (m2v(u),Vec::from(s.as_slice()),m2v(v_t)),
        _ => (vec![],vec![],vec![]),
    }
}

#[rustler::nif]
fn sum(xs: Vec<f64>) -> f64 {
    xs.iter().fold(0.0,|a, &b| a + b)
}

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn diag<'a>(env: Env<'a>,_args: Term<'a>) -> NifResult<Term<'a>> {
    //let num1: f64 = args.decode::<f64>()?;
    //Ok((num1+1.0).encode(env))
    //Ok(args)
    Ok(env.error_tuple("test failed".encode(env)))
}

rustler::init!("linalg_ruslin", [version,add,sum,transpose,matmul,inv,svd,diag]);

extern crate rustler;
use rustler::{Encoder, Env, Term, NifResult};
//use rustler::{NifEnv, NifTerm, NifError, NifDecoder, NifEncoder, NifResult};

extern crate nalgebra;
use nalgebra::base::DMatrix;
use nalgebra::linalg::SVD;


#[rustler::nif]
fn version<'a>(
    env: Env<'a>
) -> NifResult<Term<'a>> {
    let version="Version 0.1";
    return Ok(version.chars().map(|x| x as u8).collect::<Vec<u8>>().encode(env));
}

#[rustler::nif]
fn transpose<'a>(
    env: Env<'a>,
    m: Vec<Vec<f64>>
) -> NifResult<Term<'a>> {

    Ok(m2t(env, t2m(m).transpose()))
}

#[rustler::nif]
fn matmul<'a>(
    env: Env<'a>,
    a: Vec<Vec<f64>>, 
    b: Vec<Vec<f64>>
) -> NifResult<Term<'a>> {
    let a = t2m(a);
    let b = t2m(b);
    match a.nrows() == b.ncols() || b.nrows() == a.ncols() {
        true => Ok(m2t(env, a * b)),
        false => Ok(env.error_tuple(format!("{}", "bad size").encode(env)))
    }
}

#[rustler::nif]
fn inv<'a>(
    env: Env<'a>,
    m: Vec<Vec<f64>>
) -> NifResult<Term<'a>> {
    match t2m(m).try_inverse()
    {
        Some(m) => Ok(m2t(env,m)),
        None => Ok(env.error_tuple(format!("{}", "bad inv").encode(env)))
    }
}

#[rustler::nif]
fn svd<'a>(
    env: Env<'a>,
    m: Vec<Vec<f64>>
) -> NifResult<Term> {
    match t2m(m).try_svd(true, true, 0.000001, 20)
    {
        Some(SVD{u:Some(u),singular_values:s,v_t:Some(v_t)}) => Ok((m2t(env,u),Vec::from(s.as_slice()),m2t(env,v_t)).encode(env)),
        _ => Ok(env.error_tuple(format!("{}", "bad svd").encode(env)))
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

fn t2m(m: Vec<Vec<f64>>) -> DMatrix<f64> {
    let nrows=m.len();
    let ncols=m[0].len();
    let numitems=nrows*ncols;

    DMatrix::from_row_slice(nrows, ncols, &m.concat()[..numitems])
}

fn m2t(env: Env, matrix:DMatrix<f64>) -> Term {
    let ncols = matrix.ncols();
    let mut terms = Vec::new();
    for r in matrix.transpose().as_slice().chunks(ncols) {
        terms.push(Vec::from(r))
    }
    return terms.encode(env);
}


rustler::init!("linalg_ruslin", [
               version,
               add,
               sum,
               transpose,
               matmul,
               inv,
               svd,
               diag]);

extern crate image;
extern crate num;

use num::Complex;
use std::str::FromStr;

fn escape_time(c: Complex<f64>, limit: u32) -> Option<u32> {
    let mut z = Complex { re: 0.0, im: 0.0 };
    for i in 0..limit {
        z = z * z + c;
        if z.norm_sqr() > 4.0 {
            return Some(i);
        }
    }

    None
}

fn parse_pair<T: FromStr>(s: &str, separator: char) -> Option<(T, T)> {
    match s.find(separator) {
        None => None,
        Some(pos) => match (T::from_str(&s[..pos]), T::from_str(&s[pos + 1..])) {
            (Ok(n1), Ok(n2)) => Some((n1, n2)),
            _ => None,
        },
    }
}

#[test]
fn test_parse_pair() {
    assert_eq!(parse_pair::<i32>("", ','), None);
    assert_eq!(parse_pair::<i32>("1", ','), None);
    assert_eq!(parse_pair::<i32>("1j", ','), None);
    assert_eq!(parse_pair::<i32>("1,", ','), None);
    assert_eq!(parse_pair::<i32>(",1", ','), None);
    assert_eq!(parse_pair::<i32>("a,b", ','), None);
    assert_eq!(parse_pair::<i32>("1,2", ','), Some((1, 2)));
    assert_eq!(parse_pair::<i32>("1x2", ','), None);

    assert_eq!(parse_pair("1x2", 'x'), Some((1, 2)));
}

fn parse_complex(s: &str) -> Option<Complex<f64>> {
    match parse_pair(s, ',') {
        Some((re, im)) => Some(Complex { re, im }),
        None => None,
    }
}

#[test]
fn test_parse_complex() {
    assert_eq!(parse_complex("13,32"), Some(Complex { re: 13.0, im: 32.0 }));
}

fn pixel_to_complex(
    bounds: (usize, usize),
    pixel: (usize, usize),
    upper_left: Complex<f64>,
    lower_right: Complex<f64>,
) -> Complex<f64> {



    upper_left
}

fn main() {}

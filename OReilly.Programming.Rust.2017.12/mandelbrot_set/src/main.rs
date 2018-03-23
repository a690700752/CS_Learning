extern crate image;
extern crate num;

use std::io::Write;
use num::Complex;
use std::str::FromStr;
use std::fs::File;
use image::png::PNGEncoder;
use image::ColorType;

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
    let width = lower_right.re - upper_left.re;
    let height = upper_left.im - lower_right.im;

    Complex {
        re: upper_left.re + pixel.0 as f64 * width / bounds.0 as f64,
        im: upper_left.im - pixel.1 as f64 * height / bounds.1 as f64,
    }
}

#[test]
fn test_pixel_to_complex() {
    assert_eq!(
        pixel_to_complex(
            (100, 100),
            (25, 75),
            Complex { re: 0.0, im: 200.0 },
            Complex { re: 200.0, im: 0.0 },
        ),
        Complex {
            re: 50.0,
            im: 50.0,
        }
    )
}

fn render(
    canvas: &mut [u8],
    bounds: (usize, usize),
    upper_left: Complex<f64>,
    lower_right: Complex<f64>,
) {
    assert!(bounds.0 * bounds.1 == canvas.len());

    for i in 0..bounds.0 {
        for j in 0..bounds.1 {
            let p_complex = pixel_to_complex(bounds, (i, j), upper_left, lower_right);
            canvas[j * bounds.0 + i] = match escape_time(p_complex, 255) {
                Some(n) => 255 - n as u8,
                None => 0,
            }
        }
    }
}

fn save_image(
    file_name: &str,
    canvas: &[u8],
    bounds: (usize, usize),
) -> Result<(), std::io::Error> {
    let file = File::create(file_name)?;

    let encoder = PNGEncoder::new(file);
    encoder.encode(canvas, bounds.0 as u32, bounds.1 as u32, ColorType::Gray(8))?;

    Ok(())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 5 {
        writeln!(std::io::stderr(), "Usage: mandelbrot_set FILENAME SIZE UPPER_LEFT LOWER_RIGHT").unwrap();
        writeln!(std::io::stderr(), "example: {} image.png 512x512 0,1000 1000,0", args[0]).unwrap();
        std::process::exit(-1);
    }

    let bounds = parse_pair(&args[2], 'x').unwrap();
    let upper_left = parse_complex(&args[3]).unwrap();
    let lower_right = parse_complex(&args[4]).unwrap();
    let mut canvas = vec![0u8; bounds.0 * bounds.1];

    render(&mut canvas, bounds, upper_left, lower_right);
    save_image(&args[1], &canvas, bounds).unwrap();
}

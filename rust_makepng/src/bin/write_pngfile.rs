use std::{fs::File, io::BufWriter, path::Path};

use rust_makepng::make_png_data;


fn main() {
    let width = 100;
    let height = 100;
    let data = make_png_data("", width, height);

    let path = Path::new(r"C:\Users\greg\Documents\ppga\rust_makepng\test.png");
    let file = File::create(path).unwrap();
    let ref mut w = BufWriter::new(file);
    let mut encoder = png::Encoder::new(w, width as u32, height as u32);
    encoder.set_color(png::ColorType::RGB);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().unwrap();
    writer.write_image_data(&data).unwrap();
}

// Returns data in a series of [r, g, b]
pub fn make_png(s: &str, width: u32, height: u32) -> Vec<u8> {

    // TODO rendering will happen in double
    let mut pixel_data = Vec::new();
    pixel_data.reserve_exact((width as usize) * (height as usize) * 3);
    for i in 0..height {
        for j in 0..width {
            pixel_data.push(0);
            pixel_data.push(127);
            pixel_data.push(255);
        }
    }

    return pixel_data;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

use serde_json::Value;
use anyhow::{anyhow, Context, Result};

#[derive(Debug, PartialEq, Copy, Clone)]
struct ColorData {
    red: f64,
    blue: f64,
    green: f64
}

/*fn render_zeroarg<F>(f: F, x: f64, y: f64, aux: f64) -> f64
    where F: Fn(f64, f64, f64) -> f64 {
        f(x, y, aux)
}*/

trait RenderZeroArg {
    fn render(&self, x: f64, y: f64) -> f64;
}

struct RenderFnX;
impl RenderZeroArg for RenderFnX {
    fn render(&self, x: f64, _y: f64) -> f64 {
        x
    }
}
struct RenderFnY;
impl RenderZeroArg for RenderFnY {
    fn render(&self, _x: f64, y: f64) -> f64 {
        y
    }
}
struct RenderFnNum {
    num: f64
}
impl RenderZeroArg for RenderFnNum {
    fn render(&self, _x: f64, _y: f64) -> f64 {
        self.num
    }
}

fn double_to_pixel_value(d: f64) -> u8 {
    ((d + 1.0) * (255.0/2.0)) as u8
}

// Returns data in a series of [r, g, b]
pub fn make_png_data(s: &str, width: u32, height: u32) -> Result<Vec<u8>> {
    let mut pixel_data = Vec::new();
    for _i in 0..height*width {
        pixel_data.push(ColorData { red: 0.0, green: 0.0, blue: 0.0});
    }
    make_png_data_double(s, width, height, &mut pixel_data)?;

    let mut final_pixel_data = Vec::new();
    final_pixel_data.reserve_exact((width as usize) * (height as usize) * 3);
    for data in pixel_data {
        final_pixel_data.push(double_to_pixel_value(data.red));
        final_pixel_data.push(double_to_pixel_value(data.green));
        final_pixel_data.push(double_to_pixel_value(data.blue));
    }

    return Ok(final_pixel_data);
}

fn make_png_data_double(s: &str, width: u32, height: u32, data: &mut Vec<ColorData>) -> Result<()> {
    // TODO - parse s
    let j: Value = serde_json::from_str(s).context("string isn't proper JSON")?;
    let j = j.as_object().ok_or(anyhow!("json should be an object"))?;
    let function = j.get("t").ok_or(anyhow!("no 't' key"))?.as_str().ok_or(anyhow!("'t' value isn't string"))?;
    let delta_x = 2.0 / (width as f64);
    let delta_y = 2.0 / (height as f64);
    let f_zero_arg_opt : Option<Box<dyn RenderZeroArg>> = 
        match function {
            "x" => Some(Box::new(RenderFnX)),
            "y" => Some(Box::new(RenderFnY)),
            "num" => {
                let val = j.get("val").ok_or(anyhow!("'num' type has no val"))?.as_f64().ok_or(anyhow!("'num' type's val is not an f64"))?;
                Some(Box::new(RenderFnNum { num: val }))
            }
            _ => None,
        };
    if let Some(f_zero_arg) = f_zero_arg_opt {
        let mut y = -1.0;
        let mut data_index = 0;
        for _ in 0..height {
            let mut x = -1.0;
            for _ in 0..width {
                let val = f_zero_arg.render(x, y);
                data[data_index].red = val;
                data[data_index].green = val;
                data[data_index].blue = val;
                data_index += 1;
                x += delta_x;
            }
            y += delta_y;
        }
        return Ok(());
    }

    //TODO
    return Ok(());
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

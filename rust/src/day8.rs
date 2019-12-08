use std::fs::read;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;
const LAYER_SIZE: usize = WIDTH * HEIGHT;

struct Image {
  pixels: Vec<u8>,
  layers: usize,
}

impl Image {
  pub fn load(ascii_buffer: &[u8]) -> Image {
    let pixels = ascii_buffer.iter().map(|c| c - b'0').collect::<Vec<_>>();
    let layers = pixels.len() / LAYER_SIZE;
    Image { pixels, layers }
  }

  fn layer_slice(&self, layer: usize) -> &[u8] {
    let offset = LAYER_SIZE * layer;
    &self.pixels[offset..offset + LAYER_SIZE]
  }
}

fn day8a(image: &Image) {
  let fewest_zeros = (0..image.layers)
    .map(|n| image.layer_slice(n))
    .min_by_key(|buffer| buffer.iter().filter(|&&x| x == 0).count())
    .unwrap();

  let solution = fewest_zeros.iter().filter(|&&x| x == 1).count()
    * fewest_zeros.iter().filter(|&&x| x == 2).count();

  println!("Day8a: {}", solution);
}

fn day8b(image: &Image) {
  let mut full_image = [0u8; 25 * 6];

  let layer_top_to_bottom = (0..image.layers).rev().map(|n| image.layer_slice(n));

  for layer in layer_top_to_bottom {
    for (i, pixel) in layer.iter().enumerate() {
      full_image[i] = match (full_image[i], *pixel) {
        (_, 0) => 0,
        (_, 1) => 1,
        (n, 2) => n,
        _ => panic!(),
      };
    }
  }

  println!("Day8b:");
  for row in 0..HEIGHT {
    for column in 0..WIDTH {
      match full_image[row * WIDTH + column] {
        0 => print!(" "),
        1 => print!("█"),
        _ => panic!(),
      };
    }
    println!();
  }
}

pub fn day8() {
  let image_file = read("./day8.txt").unwrap();
  let image = Image::load(&image_file);

  day8a(&image);
  day8b(&image);
}

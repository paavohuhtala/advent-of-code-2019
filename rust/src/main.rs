use std::fs;
mod day5;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let input = fs::read_to_string("./day5.txt")?;
  println!("Day5a: {}", day5::day5a(&input));
  println!("Day5b: {}", day5::day5b(&input));
  Ok(())
}

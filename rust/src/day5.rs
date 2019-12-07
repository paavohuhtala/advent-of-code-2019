use crate::intcode::{parse_program, VM};
use std::fs;

fn run_with_input(program: Vec<i32>, input: i32) -> i32 {
  let mut vm = VM::new(program);
  vm.push_input(input);
  vm.run().unwrap_halt()
}

pub fn day5a(program: Vec<i32>) -> i32 {
  run_with_input(program, 1)
}

pub fn day5b(program: Vec<i32>) -> i32 {
  run_with_input(program, 5)
}

pub fn day5() {
  let input = fs::read_to_string("./day5.txt").unwrap();
  let program = parse_program(&input);

  println!("Day5a: {}", day5a(program.clone()));
  println!("Day5b: {}", day5b(program.clone()));
}

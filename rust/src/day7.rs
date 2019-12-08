#![allow(dead_code)]

use crate::intcode::{parse_program, VMState, VM};
use permutohedron::Heap;
use std::fs;

fn new_vm(program: Vec<i32>, phase_setting: i32) -> VM {
  let mut vm = VM::new(program);
  vm.push_input(phase_setting);
  vm
}

pub fn day7a(input: &str) -> i32 {
  let program = parse_program(input);

  let mut items = vec![0, 1, 2, 3, 4];

  Heap::new(&mut items)
    .map(|input| {
      let mut a = new_vm(program.clone(), input[0]);
      let mut b = new_vm(program.clone(), input[1]);
      let mut c = new_vm(program.clone(), input[2]);
      let mut d = new_vm(program.clone(), input[3]);
      let mut e = new_vm(program.clone(), input[4]);

      a.push_input(0);
      b.push_input(a.run().unwrap_halt());
      c.push_input(b.run().unwrap_halt());
      d.push_input(c.run().unwrap_halt());
      e.push_input(d.run().unwrap_halt());

      e.run().unwrap_halt()
    })
    .max()
    .unwrap()
}

pub fn day7b(input: &str) -> i32 {
  let program = parse_program(input);
  let mut items = vec![5, 6, 7, 8, 9];

  Heap::new(&mut items)
    .map(|input| {
      let mut a = new_vm(program.clone(), input[0]);
      let mut b = new_vm(program.clone(), input[1]);
      let mut c = new_vm(program.clone(), input[2]);
      let mut d = new_vm(program.clone(), input[3]);
      let mut e = new_vm(program.clone(), input[4]);

      a.push_input(0);

      loop {
        a.run();
        b.push_input(a.take_output());
        b.run();
        c.push_input(b.take_output());
        c.run();
        d.push_input(c.take_output());
        d.run();
        e.push_input(d.take_output());

        match e.run() {
          VMState::Halt(i) => return i,
          _ => {
            a.push_input(e.take_output());
          }
        }
      }
    })
    .max()
    .unwrap()
}

pub fn day7() {
  let input = fs::read_to_string("./day7.txt").unwrap();
  println!("Day7a: {}", day7a(&input));
  println!("Day7a: {}", day7b(&input));
}

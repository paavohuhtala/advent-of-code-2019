use std::collections::VecDeque;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
enum OperandMode {
  Position,
  Immediate,
}

type Operand = (OperandMode, i32);

#[derive(Debug, PartialEq, Eq)]
struct BinaryAssignInstruction {
  a: Operand,
  b: Operand,
  dest: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct ConditionalJumpInstruction {
  cond: Operand,
  then: Operand,
}

#[derive(Debug, PartialEq, Eq)]
enum Instruction {
  Add(BinaryAssignInstruction),
  Mul(BinaryAssignInstruction),
  ReadIn { dest: usize },
  WriteOut { src: Operand },
  JumpIfTrue(ConditionalJumpInstruction),
  JumpIfFalse(ConditionalJumpInstruction),
  CmpLessThan(BinaryAssignInstruction),
  CmpEqual(BinaryAssignInstruction),
  Halt,
}

#[derive(Debug, PartialEq, Eq)]
enum ExecutionResult {
  Continue,
  Halt,
}

#[derive(Debug)]
struct MachineState {
  memory: Vec<i32>,
  ip: usize,
  input_buffer: VecDeque<i32>,
  output_buffer: Vec<i32>,
}

impl MachineState {
  fn new(initial_memory: Vec<i32>) -> MachineState {
    MachineState {
      memory: initial_memory,
      ip: 0,
      input_buffer: VecDeque::new(),
      output_buffer: Vec::new(),
    }
  }

  fn read_next_word(&mut self) -> i32 {
    let word = self.memory[self.ip];
    self.ip += 1;
    word
  }

  fn read_operand(&mut self, modes: &mut VecDeque<OperandMode>) -> Operand {
    (
      modes.pop_front().unwrap_or(OperandMode::Position),
      self.read_next_word(),
    )
  }

  fn read_binary_assign(&mut self, mut modes: VecDeque<OperandMode>) -> BinaryAssignInstruction {
    BinaryAssignInstruction {
      a: self.read_operand(&mut modes),
      b: self.read_operand(&mut modes),
      dest: self.read_next_word() as usize,
    }
  }

  fn read_conditional_jump(
    &mut self,
    mut modes: VecDeque<OperandMode>,
  ) -> ConditionalJumpInstruction {
    ConditionalJumpInstruction {
      cond: self.read_operand(&mut modes),
      then: self.read_operand(&mut modes),
    }
  }

  fn fetch_operand(&self, operand: Operand) -> i32 {
    match operand {
      (OperandMode::Immediate, x) => x,
      (OperandMode::Position, x) => self.memory[x as usize],
    }
  }

  fn read_instruction(&mut self) -> Instruction {
    let op_with_modes = self.read_next_word();
    let op_with_modes_str = op_with_modes.to_string();
    let op_with_modes_chars = op_with_modes_str.as_bytes();
    let op_length = op_with_modes_chars.len();

    let mut modes = VecDeque::new();

    let op = match op_length {
      1..=2 => op_with_modes,
      _ => {
        let op = &op_with_modes_chars[(op_length - 2)..];
        modes = (&op_with_modes_chars[0..op_length - 2])
          .iter()
          .rev()
          .map(|c| match c {
            b'0' => OperandMode::Position,
            b'1' => OperandMode::Immediate,
            _ => panic!("Invalid mode: {}", c),
          })
          .collect();

        ((op[0] - b'0') * 10 + (op[1] - b'0')) as i32
      }
    };

    match op {
      1 => Instruction::Add(self.read_binary_assign(modes)),
      2 => Instruction::Mul(self.read_binary_assign(modes)),
      3 => Instruction::ReadIn {
        dest: self.read_next_word() as usize,
      },
      4 => Instruction::WriteOut {
        src: (
          modes.pop_front().unwrap_or(OperandMode::Position),
          self.read_next_word(),
        ),
      },
      5 => Instruction::JumpIfTrue(self.read_conditional_jump(modes)),
      6 => Instruction::JumpIfFalse(self.read_conditional_jump(modes)),
      7 => Instruction::CmpLessThan(self.read_binary_assign(modes)),
      8 => Instruction::CmpEqual(self.read_binary_assign(modes)),
      99 => Instruction::Halt,
      _ => panic!("Invalid op: {}", op),
    }
  }

  fn execute_instruction(&mut self, op: Instruction) -> ExecutionResult {
    match op {
      Instruction::Add(BinaryAssignInstruction { a, b, dest }) => {
        let a = self.fetch_operand(a);
        let b = self.fetch_operand(b);
        self.memory[dest] = a + b;
        ExecutionResult::Continue
      }
      Instruction::Mul(BinaryAssignInstruction { a, b, dest }) => {
        let a = self.fetch_operand(a);
        let b = self.fetch_operand(b);
        self.memory[dest] = a * b;
        ExecutionResult::Continue
      }
      Instruction::ReadIn { dest } => {
        self.memory[dest as usize] = self.input_buffer.pop_front().unwrap();
        ExecutionResult::Continue
      }
      Instruction::WriteOut { src } => {
        let src = self.fetch_operand(src);
        self.output_buffer.push(src);
        ExecutionResult::Continue
      }
      Instruction::JumpIfTrue(ConditionalJumpInstruction { cond, then }) => {
        let cond = self.fetch_operand(cond);
        if cond != 0 {
          self.ip = self.fetch_operand(then) as usize;
        }
        ExecutionResult::Continue
      }
      Instruction::JumpIfFalse(ConditionalJumpInstruction { cond, then }) => {
        let cond = self.fetch_operand(cond);
        if cond == 0 {
          self.ip = self.fetch_operand(then) as usize;
        }
        ExecutionResult::Continue
      }
      Instruction::CmpLessThan(BinaryAssignInstruction { a, b, dest }) => {
        let a = self.fetch_operand(a);
        let b = self.fetch_operand(b);
        self.memory[dest as usize] = if a < b { 1 } else { 0 };
        ExecutionResult::Continue
      }
      Instruction::CmpEqual(BinaryAssignInstruction { a, b, dest }) => {
        let a = self.fetch_operand(a);
        let b = self.fetch_operand(b);
        self.memory[dest as usize] = if a == b { 1 } else { 0 };
        ExecutionResult::Continue
      }
      Instruction::Halt => ExecutionResult::Halt,
    }
  }
}

fn run(program: &str, input: i32) -> i32 {
  let parsed_input = program
    .split(',')
    .map(|x| i32::from_str(x).unwrap())
    .collect();

  let mut vm = MachineState::new(parsed_input);
  vm.input_buffer.push_front(input);

  loop {
    let op = vm.read_instruction();
    if vm.execute_instruction(op) == ExecutionResult::Halt {
      return *vm.output_buffer.last().unwrap();
    }
  }
}

pub fn day5a(input: &str) -> i32 {
  run(input, 1)
}

pub fn day5b(input: &str) -> i32 {
  run(input, 5)
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_decode_mode() {
    let mut machine = MachineState::new(vec![1002, 4, 3, 4, 33]);
    assert_eq!(
      Instruction::Mul(BinaryAssignInstruction {
        a: (OperandMode::Position, 4),
        b: (OperandMode::Immediate, 3),
        dest: 4
      }),
      machine.read_instruction()
    );
  }
}

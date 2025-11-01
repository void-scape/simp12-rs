use crate::{
    cpu::{Alu, Mem, PcMux},
    word::Word,
};
use std::collections::HashMap;

/// The values used to initialize a [`Cpu`] with a program.
///
/// [`Cpu`]: crate::cpu::Cpu
#[derive(Debug, PartialEq, Eq)]
pub struct MachineCode {
    pub program: [Word; 256],
    pub acc: Word,
    pub pc: u8,
}

pub type Result<T> = core::result::Result<T, Error>;
/// Error for a parsing operation.
// TODO: Impl Error
#[derive(Debug, Clone, Copy)]
pub enum Error {
    InvalidPC,
    InvalidAddr,
    InvalidWord,
    //
    InvalidInstr,
    InvalidArg,
    InvalidLabel,
    InvalidArgLabel,
}

/// Parse an S12 assembly file into [`MachineCode`].
///
/// ## Format
/// Jump instructions use labels to reference blocks of instructions.
/// All other operations, excluding Halt, take an address in hexadecimal
/// as an argument.
///
/// Supported instructions:
/// - Jmp LABEL
/// - Jz LABEL
/// - Jn LABEL
/// - Load X
/// - Store X
/// - LoadI X
/// - StoreI X
/// - And X
/// - Or X
/// - Add X
/// - Sub X
/// - Halt
///
/// ## Example
/// ```toml
///     JMP ZERO_RESULT
///
/// ZERO_RESULT:
///     LOAD E2     
///     SUB E2  
///     STORE FF
///
/// ADD_MULT_TO_RESULT:
///     LOAD FE     
///     JZ DONE    
///     
///     LOAD FF    
///     ADD FD    
///     STORE FF
///     
///     LOAD FE
///     SUB 00  
///     STORE FE
///     JMP ADD_MULT_TO_RESULT
///
/// DONE:
///     HALT
/// ```
pub fn assemble(mut str: &str) -> Result<MachineCode> {
    let input = &mut str;
    let mut labels = Vec::new();
    let mut instrs = Vec::new();
    loop {
        if input.lines().next().is_some_and(|l| l.contains(':')) {
            labels.push((label(input)?, instrs.len()));
            eat_whitespace(input)?;
        } else {
            if input.is_empty() {
                break;
            }
            instrs.push(instr(input)?);
            eat_whitespace(input)?;
        }
    }

    let mut machine_code = MachineCode {
        program: [crate::word::word(0); 256],
        acc: crate::word::word(0),
        pc: 0,
    };
    for (i, instr) in instrs.iter().enumerate() {
        let word = match instr {
            Instr::Jmp(l) => {
                let (_, index) = labels
                    .iter()
                    .find(|(label, _)| label.0 == l.0)
                    .ok_or(Error::InvalidArgLabel)?;
                ((PcMux::JMP as u16) << 8) | *index as u16
            }
            Instr::Jz(l) => {
                let (_, index) = labels
                    .iter()
                    .find(|(label, _)| label.0 == l.0)
                    .ok_or(Error::InvalidArgLabel)?;
                ((PcMux::JZ as u16) << 8) | *index as u16
            }
            Instr::Jn(l) => {
                let (_, index) = labels
                    .iter()
                    .find(|(label, _)| label.0 == l.0)
                    .ok_or(Error::InvalidArgLabel)?;
                ((PcMux::JN as u16) << 8) | *index as u16
            }
            Instr::Load(x) => ((Mem::LOAD as u16) << 8) | *x as u16,
            Instr::Store(x) => ((Mem::STORE as u16) << 8) | *x as u16,
            Instr::LoadI(_) => {
                todo!("LOADI");
            }
            Instr::StoreI(_) => {
                todo!("STOREI");
            }
            Instr::And(x) => ((Alu::AND as u16) << 8) | *x as u16,
            Instr::Or(x) => ((Alu::OR as u16) << 8) | *x as u16,
            Instr::Add(x) => ((Alu::ADD as u16) << 8) | *x as u16,
            Instr::Sub(x) => ((Alu::SUB as u16) << 8) | *x as u16,
            Instr::Halt => 0b1111 << 8,
        };
        machine_code.program[i] = crate::word::word(word);
    }
    Ok(machine_code)
}

#[derive(PartialEq, Eq, Hash)]
struct Label(String);

fn label(input: &mut &str) -> Result<Label> {
    eat_whitespace(input)?;
    input.lines().next().ok_or(Error::InvalidLabel)?;
    match input.find(':') {
        Some(index) => {
            let label = input[..index].to_string();
            *input = &input[index + 1..];
            Ok(Label(label))
        }
        None => Err(Error::InvalidLabel),
    }
}

fn label_str(input: &mut &str) -> Result<Label> {
    eat_whitespace(input)?;
    input.lines().next().ok_or(Error::InvalidLabel)?;
    match input.find(|c: char| c.is_whitespace()) {
        Some(index) => {
            let label = input[..index].to_string();
            *input = &input[index + 1..];
            Ok(Label(label))
        }
        None => Err(Error::InvalidLabel),
    }
}

#[derive(PartialEq, Eq, Hash)]
enum Instr {
    Jmp(Label),
    Jz(Label),
    Jn(Label),
    Load(u8),
    Store(u8),
    #[allow(unused)]
    LoadI(u8),
    #[allow(unused)]
    StoreI(u8),
    And(u8),
    Or(u8),
    Add(u8),
    Sub(u8),
    Halt,
}

fn instr(input: &mut &str) -> Result<Instr> {
    eat_whitespace(input)?;
    for (i, c) in input.char_indices() {
        if c.is_whitespace() {
            let instr = input[..i].to_ascii_lowercase();
            *input = &input[i..];
            match &*instr {
                "halt" => {
                    return Ok(Instr::Halt);
                }
                "jmp" => return Ok(Instr::Jmp(label_str(input)?)),
                "jz" => return Ok(Instr::Jz(label_str(input)?)),
                "jn" => return Ok(Instr::Jn(label_str(input)?)),
                _ => {}
            }
            eat_whitespace(input)?;
            let arg = arg(input)?;
            let instr = match &*instr {
                "load" => Instr::Load(arg),
                "store" => Instr::Store(arg),
                "loadi" => Instr::LoadI(arg),
                "storei" => Instr::StoreI(arg),
                "and" => Instr::And(arg),
                "or" => Instr::Or(arg),
                "add" => Instr::Add(arg),
                "sub" => Instr::Sub(arg),
                _ => return Err(Error::InvalidInstr),
            };
            return Ok(instr);
        }
    }
    Err(Error::InvalidInstr)
}

fn arg(input: &mut &str) -> Result<u8> {
    let (arg, next_input) = input.split_at(2);
    *input = next_input;
    if arg.chars().all(|c| c.is_ascii_hexdigit()) {
        u8::from_str_radix(arg, 16).map_err(|err| panic!("{err}"))
    } else {
        Err(Error::InvalidArg)
    }
}

/// Parse a memory file into [`MachineCode`].
///
/// ## Format
/// The first line should contain initial PC and ACC values. They must be
/// represented in binary. The following lines are the program memory. Each
/// line is started with a hexadecimal address, followed by a `12`-bit word.
///
/// ```toml
/// 00000000 000000000000
/// 00 000000000001
/// 01 010011100010
/// ```
pub fn memory_file(mut bytes: &[u8]) -> Result<MachineCode> {
    let input = &mut bytes;
    let pc = pc(input)?;
    aeat_whitespace(input)?;
    let acc = word(input)?;
    aeat_whitespace(input)?;

    let mut mc = MachineCode {
        program: [crate::word::word(0); 256],
        acc,
        pc,
    };

    while !input.is_empty() {
        let addr = addr(input)?;
        aeat_whitespace(input)?;
        mc.program[addr as usize] = word(input)?;
        aeat_whitespace(input)?;
    }

    Ok(mc)
}

fn pc(input: &mut &[u8]) -> Result<u8> {
    let (pc, next_input) = input.split_at(8);
    *input = next_input;
    if pc.iter().all(|b| char::is_ascii_digit(&(*b as char))) {
        u8::from_ascii_radix(pc, 2).map_err(|err| panic!("{err}"))
    } else {
        Err(Error::InvalidPC)
    }
}

fn addr(input: &mut &[u8]) -> Result<u8> {
    let (addr, next_input) = input.split_at(2);
    *input = next_input;
    if addr.iter().all(|b| char::is_ascii_hexdigit(&(*b as char))) {
        u8::from_ascii_radix(addr, 16).map_err(|err| panic!("{err}"))
    } else {
        Err(Error::InvalidAddr)
    }
}

fn word(input: &mut &[u8]) -> Result<Word> {
    let (word_bytes, next_input) = input.split_at(12);
    *input = next_input;
    if word_bytes
        .iter()
        .all(|b| char::is_ascii_digit(&(*b as char)))
    {
        u16::from_ascii_radix(word_bytes, 2)
            .map(|w| crate::word::word(w))
            .map_err(|err| panic!("{err}"))
    } else {
        Err(Error::InvalidWord)
    }
}

fn eat_whitespace(input: &mut &str) -> Result<()> {
    let mut start_comment = false;
    loop {
        match input.chars().next() {
            Some(c) => {
                if c.is_whitespace() {
                    *input = &input[c.len_utf8()..];
                } else if c == '/' {
                    if start_comment {
                        match input.find('\n') {
                            Some(index) => {
                                *input = &input[index..];
                            }
                            None => {
                                *input = "";
                                return Ok(());
                            }
                        }
                    }
                    start_comment = !start_comment;
                } else {
                    return Ok(());
                }
            }
            None => {
                *input = "";
                return Ok(());
            }
        }
    }
}

fn aeat_whitespace(input: &mut &[u8]) -> Result<()> {
    while input
        .first()
        .is_some_and(|b| char::is_whitespace(*b as char))
    {
        *input = &input[1..];
    }
    Ok(())
}

pub struct MachineCodeStatistics {
    pub instr_mix: String,
    pub number_of_instructions: usize,
}

/// Parse a memory file and report machine code statistics.
pub fn memory_file_statistics(mut bytes: &[u8]) -> Result<MachineCodeStatistics> {
    let input = &mut bytes;
    let _pc = pc(input)?;
    aeat_whitespace(input)?;
    let _acc = word(input)?;
    aeat_whitespace(input)?;

    let mut instr_mix = String::new();
    let mut instr_map: HashMap<u8, usize> = HashMap::new();
    let mut number_of_instructions = 0;
    while !input.is_empty() {
        let _addr = addr(input)?;
        aeat_whitespace(input)?;
        let instr = word(input)?;
        aeat_whitespace(input)?;
        number_of_instructions += 1;

        *instr_map
            .entry((instr.into_inner() >> 8) as u8)
            .or_default() += 1;
    }

    for (instr, count) in instr_map.into_iter() {
        let mix = count as f32 / number_of_instructions as f32;
        let instr = opcode_as_str(instr);
        instr_mix.push_str(&format!("{}\t{:.2}%\n", instr, mix * 100.0));
    }

    Ok(MachineCodeStatistics {
        instr_mix,
        number_of_instructions,
    })
}

/// Convert the high nibble of an instruction word into the instruction identifier.
pub fn opcode_as_str(opcode: u8) -> &'static str {
    match opcode {
        Alu::ADD => "ADD",
        Alu::SUB => "SUB",
        Alu::AND => "AND",
        Alu::OR => "OR",
        PcMux::JMP => "JMP",
        PcMux::JZ => "JZ",
        PcMux::JN => "JN",
        Mem::LOAD => "LOAD",
        Mem::STORE => "STORE",
        0b1111 => "HALT",
        _ => "UNKNOWN",
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mult2int() {
        let bytes = include_bytes!("mult2int_invalid.txt");
        assert!(memory_file(bytes.as_slice()).is_err());

        let bytes = include_bytes!("mult2int.txt");
        assert!(memory_file(bytes.as_slice()).is_ok());

        let str = include_str!("mult2int.asm");
        assert!(assemble(str).is_ok());

        assert_eq!(
            assemble(str).unwrap(),
            memory_file(bytes.as_slice()).unwrap()
        )
    }
}

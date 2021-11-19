#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![deny(rust_2018_idioms)]
#![allow(clippy::upper_case_acronyms)]

use crate::chunk::{writeChunk, freeChunk, addConstant};
use crate::chunk::OpCode::*;
use crate::vm::*;

mod chunk;
mod debug;
mod value;
mod vm;
mod scanner;
mod compiler;
mod strings;
mod common;
mod ast;

use std::fs ;
use std::path::*;
use std::env ;
use std::io::{self,stdin,stdout,Write};
use std::fmt::Error;
use std::borrow::{BorrowMut, Borrow};
use clap::{Arg, App};


fn main() -> ! {

    let app = App::new("Coyote")
        .version("1.0")
        .author("Claude Seidman claude@intellixus.com")
        .arg(Arg::with_name("file")
            .short("f") // allow --file
            .takes_value(true)
            .help("Coyote program to run")
            .required(false))
        .arg(Arg::with_name("debug")
            .short("d")
            .multiple(true)
            .help("Sets the level of debugging information"))
        .get_matches();

    // Extract the actual name
    if let Some(filename) = app.value_of("file") {
        //if let Some(debugging) = app.value_of("debug") {
            runFile(filename) ;
        //} else {
        //    runFile(filename) ;
        //}
    } else {
        Repl() ;
    }

    std::process::exit(0) ;
}

pub fn runFile(filename: &str) {
    let mut vm = VM::new() ;
    let code = ReadSource(filename) ;
    vm.interpret(code) ;
}

pub fn Repl() {

    let mut vm = VM::new() ;
    loop {
        let mut line=String::new();
        print!("> ");

        let _=stdout().flush();
        stdin().read_line(&mut line).unwrap();

        if line == "\n" {
            return;
        }

        vm.interpret(line);
    }
}

pub fn ReadSource(filePath: &str) -> String {
    if !Path::new(filePath).exists() {
        println!("Unable to open {}",filePath) ;
        panic!() ;
    }

    fs::read_to_string(filePath).expect("Unable to read source file")
}





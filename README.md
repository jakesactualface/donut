# Welcome to Donut!

Donut is an interpreted programming language that I wrote myself, with the help of Thorsten Ball's book, _Writing An Interpreter In Go_. If you're wondering why Donut is written in Rust when the book I referenced refers to Go, that's because I like writing things in Rust and you can't stop me.

If that sounds like a fun project to you, I would recommend checking out https://interpreterbook.com/ to learn more about Thorsten's eBook.

# Installation

## Requirements

- Rust
  - [Installation instructions](https://www.rust-lang.org/tools/install)

While I currently have nothing in place to distribute binaries of Dough (the Donut interpreter), it's pretty painless to compile yourself as long as you have a valid Rust installation.

1. Clone this repository
2. Install using Cargo
   - Use the following command within the `donut` project directory:
     - ```sh
       cargo install --path .
       ```
3. Execute the `dough` binary to run the interpreter

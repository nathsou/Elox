# Elox

Elox for Extended-lox, is a superset of the lox toy language from Bob Nystrom's book [_Crafting Interpreters_](http://www.craftinginterpreters.com/).

It is an interpreted, dynamically typed, objected oriented programming language.

You can test it [here](https://nathsou.github.io/Elox/web/dist/index.html)

Its main additions to lox are :

- Native Arrays
- String concatenation of any variable types allowed
- Modulo '%' operator available
- Usual assignment shorthands: +=, -=, ++, --, *=, /=, %=
- Default valued function parameters
- Rest parameters
- Anonymous functions allowed

# Running

Elox is written in Rust and can thus target your computer's architecture and [WebAssembly](https://webassembly.org/).

## Compiling and running

In the project folder: 

```bash
$ cargo run --release [file.elox]
```

## Compiling to wasm

Using [wasm-pack](https://rustwasm.github.io/wasm-pack/) in the project folder:

```bash
$ wasm-pack build
```

The demo website using the compiled wasm module can be run using:

```bash
$ cd web
$ npm run start
```

## Todo

- [ ] Arrow functions
- [ ] standard library
 - [ ] Implement [traits](https://www.wikiwand.com/en/Trait_(computer_programming))
 - [ ] Write a compiler targetting wasm directly
 - [ ] Optional type annotations used by a static type checker
 - [ ] Function overloading
 - [ ] 'const' keyword
- [ ] Replace 'nil' with Option\<T> or other? => match and enums?
- [ ] bundler to import other elox files
- [ ] extern code execution (C or JS)

## Examples

Demos are available in the /demos folder and can be [run online](https://nathsou.github.io/Elox/web/dist/index.html).

```javascript
// Pseudo Random Number Generator
class PRNG {
  init(seed = clock() * 1000) {
    this.a = 25214903917;
    this.c = 11;
    this.m = 281474976710656;
    this.seed = seed;
  }

  next() {
    this.seed = (this.a * this.seed + this.c) % this.m;
    return this.seed / this.m;
  }
}
 
var prng = PRNG();
var nums = [];

for (var i = 0; i < 10; i++) {
   nums.push(prng.next());
}

print nums;
```
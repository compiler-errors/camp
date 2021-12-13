# Camp Programming Language

This is a toy programming language that I've called Camp. It's modeled after Rust, and I intend for it to feature:
* H-M type inferencing and generics
* Trait and impl-based system (possibly implemented like Chalk, or else like rustc's current trait solver)
* Rust-like borrow checker system (hopefully implemented like Polonius)

It comprises of a demand-driven system of queries between layers of the compiler using [`salsa`](https://github.com/salsa-rs/salsa). Its parser is inspired by [`syn`](https://github.com/dtolnay/syn). I eventually intend to base the type checker off of rustc and Chalk, and the borrow checker off of Polonius.

Eventually, I would like for this compiler to be used as subject material for educational purposes, such as a series of blog posts explaining the layout of a reasonably modern and non-minimalistic compiler, or as a small course where students can fill out portions of the compiler to learn about how it all works together.

## Disclaimer

Very WIP, definitely not cleaned up yet. It doesn't do anything useful at all.

## License

Licensed under the [MIT license](LICENSE-MIT).
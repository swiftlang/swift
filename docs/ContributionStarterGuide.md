# Contribution Starter Guide

## Introduction
This guide aims to help you start your first PR on Swift! The guide contains links for blogs and talks that are ralated on how to start contribution to Swift. 

## Compiler Pipeline
  It's very important that we understand the general pipeline of the compiler. This is useful to understand where new changes or where to fix things should go. In each section, I'll show you an example of what the process is doing, and at the end of each section I'll give you an error that happens in that stage.


### Lexer
<details>
<summary>
This is the first part in the compilation process where the compiler breaks down your source code into what we call tokens.</summary>

```
// Source code
let x = 16

// The Lexer turns this into tokens
[kw_let, identifier("x"), equal, integer_literal(16)]
// kw means keyword
```
The goal of the lexer is to be able to produce this sequence of tokens for the parser to build an AST (Abstract Syntax Tree) from it.

An example error that can happen during this stage:
```
// error: unterminated string literal
let x = "
        ^
```
The lexer wants to make a `string_literal` token, but notices that there is not a matching "

#### Links
Lexer.h (Header file) - https://github.com/apple/swift/blob/master/include/swift/Parse/Lexer.h </br>
Lexer.cpp (Implementation) - https://github.com/apple/swift/blob/master/lib/Parse/Lexer.cpp </br>
Lexer Diagnostics (errors, warnings, notes) - https://github.com/apple/swift/blob/master/include/swift/AST/DiagnosticsParse.def#L40-L197 </br>
Lexical Analysis - https://en.wikipedia.org/wiki/Lexical_analysis
</details>

### Parser
<details>
<summary>
The Parser takes this tokens and tries build the AST (Abstract Syntax Tree). This is how the compiler actually is able to understand your source code and make sense of it.</summary>

```
// Tokens from lexer
[kw_let, identifier("x"), equal, integer_literal(16)]

// The Parser transforms this into the AST
let x = 16
```

Here we can see that we actually understand what the parser formed here. At this stage, we can also start adding some flags to the compiler to see some more of how the compiler understands and lowers our source code.

`$ swiftc -dump-parse example.swift`

This will dump the basic pre-typechecked AST that the parser has built from our source code:

```
(source_file
  (top_level_code_decl
    (brace_stmt
      (pattern_binding_decl
        (pattern_named 'x')
        (integer_literal_expr type='<null>' value=16))))
  (var_decl "x" type='<null type>' let storage_kind=stored))
```

We can see our variable declaration for x and we can also see that our `integer_literal(16)` has turned into an expression for something called a pattern binding declaration. (For now we don't really need to understand what that means to really get the gist of what is happening here. But, a pattern binding declaration is basically binding an expression to a pattern. In this case that pattern is `x`).

An example error that happen during this stage:
```
// Tokens produced from lexer for this source code
// [kw_struct, l_brace, r_brace]

// error: expected identifier in struct declaration
struct {}
       ^
```

The parser sees this `kw_struct` token from the lexer and wants to make a struct declaration from it, but the next token is not an identifier to name the structure, but rather the next token is `l_brace` (left brace).

#### Links
Parse Headers - https://github.com/apple/swift/tree/master/include/swift/Parse </br>
Parse Implementation files - https://github.com/apple/swift/tree/master/lib/Parse </br>
Parse diagnostics - https://github.com/apple/swift/blob/master/include/swift/AST/DiagnosticsParse.def </br>
Parsing - https://en.wikipedia.org/wiki/Parsing </br>
Abstract Syntax Tree - https://en.wikipedia.org/wiki/Abstract_syntax_tree
</details>

### Sema (Semantic Analysis)
<details>
<summary>
Sema is our next step in the compilation process. This includes things like the Type Checker and the Constraint System. There are a lot of goals that this step has to accomplish like assign types to expressions, ensure our structs are conforming to protocols properly, automatically deriving protocol conformance for types, or synthesizing code like the memberwise initializer for structs, and many more.
</summary>

```
// AST from the parser
let x = 16

// Sema refined AST
internal let x: Int = 16
```
I know, it doesn't seem like Sema did much in this step, but there is another flag we can pass to the compiler now!

`$ swiftc -dump-ast example.swift`

This will dump the typechecked AST.
```
(source_file
  (top_level_code_decl
    (brace_stmt
      (pattern_binding_decl
        (pattern_named type='Int' 'x')
        (call_expr implicit type='Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] nothrow arg_labels=_builtinIntegerLiteral:
          (constructor_ref_call_expr implicit type='(_MaxBuiltinIntegerType) -> Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] nothrow
            (declref_expr implicit type='(Int.Type) -> (_MaxBuiltinIntegerType) -> Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] decl=Swift.(file).Int.init(_builtinIntegerLiteral:) function_ref=single)
            (type_expr implicit type='Int.Type' location=<source>:1:9 range=[<source>:1:9 - line:1:9] typerepr='Int'))
          (tuple_expr implicit type='(_builtinIntegerLiteral: Int2048)' location=<source>:1:9 range=[<source>:1:9 - line:1:9] names=_builtinIntegerLiteral
            (integer_literal_expr type='Int2048' location=<source>:1:9 range=[<source>:1:9 - line:1:9] value=16))))))
  (var_decl "x" type='Int' interface type='Int' access=internal let storage_kind=stored))
```
There's a lot happening here! Hang in there with me, lets try to decipher this a little bit. Lets start simple:

`(var_decl "x" type='Int' interface type='Int' access=internal let storage_kind=stored))`

We can see our variable declaration for x still and we notice that there is now a type, `Int`, and we see the access control level for it as well, `internal`. That wasn't too bad...

Now for the good stuff!
```
(pattern_binding_decl
  (pattern_named type='Int' 'x')
  (call_expr implicit type='Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] nothrow arg_labels=_builtinIntegerLiteral:
    (constructor_ref_call_expr implicit type='(_MaxBuiltinIntegerType) -> Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] nothrow
      (declref_expr implicit type='(Int.Type) -> (_MaxBuiltinIntegerType) -> Int' location=<source>:1:9 range=[<source>:1:9 - line:1:9] decl=Swift.(file).Int.init(_builtinIntegerLiteral:) function_ref=single)
      (type_expr implicit type='Int.Type' location=<source>:1:9 range=[<source>:1:9 - line:1:9] typerepr='Int'))
    (tuple_expr implicit type='(_builtinIntegerLiteral: Int2048)' location=<source>:1:9 range=[<source>:1:9 - line:1:9] names=_builtinIntegerLiteral
      (integer_literal_expr type='Int2048' location=<source>:1:9 range=[<source>:1:9 - line:1:9] value=16))))))
```
We see that we our simple `integer_literal_expr` from parsing has now transformed into what appears to be a `call_expr`. I'll help you out a bit and spell this out in Swift and hopefully you can look at this and kind of understand the structure.

`internal let x: Int = Int(_builtinIntegerLiteral: 16)`

This is where we see the tight relationship between the compiler and the standard library. You can actually find this initializer for `Int` here: https://github.com/apple/swift/blob/master/stdlib/public/core/IntegerTypes.swift.gyb#L1111

This is a trivial example about what Sema does during this stage, but I encourage you to check it out further as this is my favorite step in the pipeline!

An example error during this stage:
```
// error: cannot convert value of type 'Bool' to specified type 'Int'
let x: Int = true
             ^~~~
```
We assigned a Bool value to a variable with the Int type and thankfully the type checker caught this for us!

#### Links
Sema - https://github.com/apple/swift/tree/master/lib/Sema </br>
Semantic Analysis - https://en.wikipedia.org/wiki/Semantic_analysis_(compilers) (This wiki isnt too helpful, but it includes some useful links to things like type checker)
</details>

### SIL (Swift Intermediate Language)
<details>
<summary>
SIL provides things like high level optimizations for our Swift code, emitting dataflow diagnostics, and a many more.
</summary>

```
// Type checked AST from Sema
internal let x: Int = 16

// SIL
sil_stage canonical

import Builtin
import Swift
import SwiftShims

let x: Int

sil_global hidden [let] @$S6output1xSivp : $Int

sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
bb0(%0 : $Int32, %1 : $UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>):
  alloc_global @$S6output1xSivp // id: %2
  %3 = global_addr @$S6output1xSivp : $*Int // user: %6
  %4 = integer_literal $Builtin.Int64, 16 // user: %5
  %5 = struct $Int (%4 : $Builtin.Int64) // user: %6
  store %5 to %3 : $*Int // id: %6
  %7 = integer_literal $Builtin.Int32, 0 // user: %8
  %8 = struct $Int32 (%7 : $Builtin.Int32) // user: %9
  return %8 : $Int32 // id: %9
} // end sil function 'main'

sil public_external [transparent] [serialized] @$SSi22_builtinIntegerLiteralSiBi2048__tcfC : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int {
bb0(%0 : $Builtin.Int2048, %1 : $@thin Int.Type):
  %2 = builtin "s_to_s_checked_trunc_Int2048_Int64"(%0 : $Builtin.Int2048) : $(Builtin.Int64, Builtin.Int1) // user: %3
  %3 = tuple_extract %2 : $(Builtin.Int64, Builtin.Int1), 0 // user: %4
  %4 = struct $Int (%3 : $Builtin.Int64) // user: %5
  return %4 : $Int // id: %5
} // end sil function '$SSi22_builtinIntegerLiteralSiBi2048__tcfC'
```

There is a lot to cover here. There's a lot of interesting parts that we could go over, but for the sake of having a basic understanding of what is happening here I'll go over just the parts in the main function.

`
sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32
`

If this function signature looks similar to anyone, you know exactly what this looks like.

`int main(int argc, char **argv)`

for the body of the main function:
```
alloc_global @$S6output1xSivp // id: %2
%3 = global_addr @$S6output1xSivp : $*Int // user: %6
%4 = integer_literal $Builtin.Int64, 16 // user: %5
%5 = struct $Int (%4 : $Builtin.Int64) // user: %6
store %5 to %3 : $*Int // id: %6
%7 = integer_literal $Builtin.Int32, 0 // user: %8
%8 = struct $Int32 (%7 : $Builtin.Int32) // user: %9
return %8 : $Int32 // id: %9
```

Hopefully I can translate directly what this does for a quick overview. First we allocate a global variable, `x`, and then we store the address of it in `%3`. We then construct an `Int` with the value of 16 which is stored in `%5`. After we construct the integer, we store the value of it to the address of the global variable `x`. Then the last three lines are simply `return 0` for the main function.

It's important to note that there are two steps in this SIL step, SILGen and SIL. SILGen takes the AST from Sema and produces a raw version of SIL that is handed off to SIL for mandatory optimizations and diagnostics which then produces a canonical version of the SIL.

We also get new flags we can pass here as well!

```
// This emits the raw version of SIL from SILGen
$ swiftc -emit-silgen example.swift

// This emits the canonical version of SIL
$ swiftc -emit-sil example.swift
```

An example error at this stage:
```
// error: missing return in a function expected to return 'Int'
func add(_ x: Int, _ y: Int) -> Int {
  let sum = x + y
}
^
```
This is part of the dataflow diagnostics that I mentioned earlier. SIL determines that there is no "terminator", or return instruction in this case, for this function and emits an error letting the developer know.

#### Links
SIL Header files - https://github.com/apple/swift/tree/master/include/swift/SIL </br>
SIL Diagnostics - https://github.com/apple/swift/blob/master/include/swift/AST/DiagnosticsSIL.def </br>
SIL Optimizer Header files - https://github.com/apple/swift/tree/master/include/swift/SILOptimizer </br>
SILGen Implementation files - https://github.com/apple/swift/tree/master/lib/SILGen </br>
SIL Implementation files - https://github.com/apple/swift/tree/master/lib/SIL </br>
SIL Optimizer Implementation files - https://github.com/apple/swift/tree/master/lib/SILOptimizer </br>
SIL Documentation - https://github.com/apple/swift/blob/master/docs/SIL.rst (I highly recommend reading this as it will do a far better job explaining SIL and its purpose) </br>
SIL Programmer's Manual (Work in Progress) - https://github.com/apple/swift/blob/master/docs/SILProgrammersManual.md </br>
Video with Joe Groff and Chris Lattner at LLVM conf - https://www.youtube.com/watch?v=Ntj8ab-5cvE
</details>

### IR (LLVM IR Generation)
<details>
<summary>
This is the final step (at least for the Swift compiler!) in the compilation process. The goal of this step is to take our refined and optimized SIL and emit LLVM IR (LLVM Intermediate Representation).
</summary>

I won't copy the SIL from the last step here, but I will show you the IR produced from this stage (not all of the IR, but the relevant stuff).
```
%TSi = type <{ i64 }>

define protected i32 @main(i32, i8**) #0 !dbg !25 {
  %2 = bitcast i8** %1 to i8*
  store i64 16, i64* getelementptr inbounds (%TSi, %TSi* @"$S6output1xSivp", i32 0, i32 0), align 8, !dbg !30
  ret i32 0, !dbg !30
}
```

This isn't too bad! First we define `%TSi` as a type containing `i64`, which is a 64 bit integer in LLVM. I'll spell out what exactly what `%TSi` is in Swift for a better understanding:

```
public struct Int {
  var _value: Builtin.Int
}
```
`%TSi` is just our `Int` type in Swift!

Next, we define our main method and `store i64 16` into our global variable `x` and finally `return i32 0` (return 0).

And, as always we get new flags to emit llvm ir for us:

`$ swiftc -emit-ir example.swift`

From here, we pass this IR to LLVM to emit a native binary for us. If you want to see the assembly produced from LLVM:

`$ swiftc -emit-assembly example.swift`

### Links
IRGen - https://github.com/apple/swift/tree/master/lib/IRGen </br>
IRGen Diagnostics - https://github.com/apple/swift/blob/master/include/swift/AST/DiagnosticsIRGen.def 
</details>

> The Compiler Pipeline is originally written by Alejandro Alonso on the [Swift Forums](https://forums.swift.org/t/what-should-i-learn-if-i-want-to-contribute-to-the-swift-compiler/18144/5?u=hassaneldesouky).

## Blogs & Articles

### Getting started
- [Getting Started with Swift Compiler Development](https://modocache.io/getting-started-with-swift-development) by BRIAN GESIAK
- [Contributing to Swift — Part 1](https://medium.com/kinandcartacreated/contributing-to-swift-part-1-ea19108a2a54) by Suyash Srijan
- [Contributing to Swift — Part 2](https://medium.com/kinandcartacreated/contributing-to-swift-part-2-efebcf7b6c93) by Suyash Srijan

### Tips
- [Getting started tips](https://forums.swift.org/t/getting-started-with-swift-compiler-development/31502/2?u=hassaneldesouky) by Varun Gandhi
- [Workflow tips](https://forums.swift.org/t/need-a-workflow-advice/12536/14) by Robert Widmann

## Talks
- [What is LLVM? What Makes Swift Possible?](https://www.youtube.com/watch?v=KA8hFBh2eiw)
- [Contributing to Swift Compiler](https://www.youtube.com/watch?v=HAXJsgYniqE&t=127s)
- [try! Swift Tokyo 2016 - Contributing to Open Source Swift](https://www.youtube.com/watch?v=Ysa2n8ZX-YY)
- [Harlan Haskins & Robert Widmann - Becoming An Effective Contributor to Swift](https://www.youtube.com/watch?v=oGJKsp-pZPk)
- [Contributing to Swift for the First Time - Robert Pieta](https://www.youtube.com/watch?v=51j7TrFNKiA&t=312s)
- [Compilers Aren't Magic, So Let's Build One in Swift](https://www.youtube.com/watch?v=XkjySn0nwzQ&t=983s)

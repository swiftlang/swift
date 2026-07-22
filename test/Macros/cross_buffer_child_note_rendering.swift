// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: not env TMPDIR=%t %target-swift-frontend -swift-version 5 -typecheck %t/test.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -diagnostic-style=llvm 2>&1 | %PathSanitizingDiff --sanitize-regex "MACROBUFFER=@__swiftmacro\\S*.swift" %t/llvm-render.expected
// RUN: not env TMPDIR=%t %target-swift-frontend -swift-version 5 -typecheck %t/test.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -diagnostic-style=swift 2>&1 | %PathSanitizingDiff %t/swift-syntax-render.expected
// RUN: env TMPDIR=%t %target-swift-frontend -swift-version 5 -typecheck %t/test.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -verify

// Ensure that child notes in other buffers are rendered properly.

//--- test.swift
@freestanding(declaration, names: named(A), named(B), named(foo), named(addOne))
macro defineDeclsWithKnownNames() = #externalMacro(module: "MacroDefinition", type: "DefineDeclsWithKnownNamesMacro")

// This expands to a top-level 'var foo: Int' living in a macro expansion buffer.
// expected-expansion@+3:1{{
//   expected-note@13{{'foo' previously declared here}}
// }}
#defineDeclsWithKnownNames

// expected-error@+1{{invalid redeclaration of 'foo'}}
var foo: Int { 2 }

//--- llvm-render.expected
TMP_DIR/test.swift:11:5: error: invalid redeclaration of 'foo'
var foo: Int { 2 }
    ^
TMP_DIR/swift-generated-sources/MACROBUFFER:13:5: note: 'foo' previously declared here
var foo: Int {
    ^
//--- swift-syntax-render.expected
TMP_DIR/test.swift:11:5: error: invalid redeclaration of 'foo'
 6 | //   expected-note@13{{'foo' previously declared here}}
 7 | // }}
 8 | #defineDeclsWithKnownNames
   +--- macro expansion #defineDeclsWithKnownNames ---------------------
   |11 |   }
   |12 | }
   |13 | var foo: Int {
   |   |     `- note: 'foo' previously declared here
   |14 |     1
   |15 | }
   +--------------------------------------------------------------------
 9 | 
10 | // expected-error@+1{{invalid redeclaration of 'foo'}}
11 | var foo: Int { 2 }
   |     `- error: invalid redeclaration of 'foo'
12 | 
13 | 

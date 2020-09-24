// RUN: not %target-swift-frontend -diagnostic-style=swift -typecheck %/s 2>&1 | %FileCheck %s

1 + 2

func foo(a: Int, b: Int) {
  a + b
}

foo(b: 1, a: 2)


func baz() {
  bar(a: "hello, world!")
}

struct Foo {
  var x: Int
  var x: Int
}

func bar(a: Int) {}
func bar(a: Float) {}


func bazz() throws {

}
bazz()

struct A {}
extension A {
  let x: Int = { 42 }
}

let abc = "👍

let x = {
  let y = 1
  return y
}

struct B: Decodable {
  let a: Foo
}

// The line below is indented with tabs, not spaces.
			foo(b: 1, a: 2)

let 👍👍👍 = {
  let y = 1
  return y
}

// Multi-line fix-its
foo(b: 1,
    a: 2)

foo(b:
    1,
    a:
    2)

foo(b:
    1,
    a: 2)


// Test fallback for non-ASCII characters.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:11
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | let abc = "👍
// CHECK:             | --> error: unterminated string literal
// CHECK: [[#LINE+1]] |

// Test underlining.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:3
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | 1 + 2
// CHECK:             | ~   ~
// CHECK:             |   ^ warning: result of operator '+' is unused
// CHECK: [[#LINE+1]] |

// Test inline fix-it rendering.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:11
// CHECK:  [[#LINE-1]] |
// CHECK:  [[#LINE]]   | foo(a: 2, b: 1, a: 2)
// CHECK:              |     ++++++~~~~------
// CHECK:              |                 ^ error: argument 'a' must precede argument 'b' [remove ', a: 2' and insert 'a: 2, ']
// CHECK: [[#LINE+1]]  |


// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:7
// CHECK: [[#LINE-2]] | struct Foo {
// CHECK: [[#LINE-1]] |   var x: Int
// CHECK:             |       ^ note: 'x' previously declared here
// CHECK: [[#LINE]]   |   var x: Int
// CHECK:             |       ^ error: invalid redeclaration of 'x'
// CHECK: [[#LINE+1]] | }

// Test out-of-line fix-its on notes.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:1
// CHECK: [[#LINE-1]] | }
// CHECK: [[#LINE]]   | bazz()
// CHECK:             | ~~~~~~
// CHECK:             | ^ error: call can throw but is not marked with 'try'
// CHECK:             | ^ note: did you mean to use 'try'? [insert 'try ']
// CHECK:             | ^ note: did you mean to handle error as optional value? [insert 'try? ']
// CHECK:             | ^ note: did you mean to disable error propagation? [insert 'try! ']
// CHECK: [[#LINE+1]] |


// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:7
// CHECK: [[#LINE-1]] | extension A {
// CHECK: [[#LINE]]   |   let x: Int = { 42 }
// CHECK:             |       ^ error: extensions must not contain stored properties
// CHECK: [[#LINE+1]] | }

// Test complex out-of-line fix-its.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:16
// CHECK: [[#LINE-1]] | extension A {
// CHECK: [[#LINE]]   |   let x: Int = { 42 }()
// CHECK:             |                ~~~~~~++
// CHECK:             |                ^ error: function produces expected type 'Int'; did you mean to call it with '()'?
// CHECK:             |                ^ note: Remove '=' to make 'x' a computed property [remove '= ' and replace 'let' with 'var']
// CHECK: [[#LINE+1]] | }

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:9
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | let x = { () -> Result in
// CHECK:             |          +++++++++++++++++
// CHECK:             |         ^ error: unable to infer complex closure return type; add explicit type to disambiguate
// CHECK: [[#LINE+1]] |   let y = 1

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:8
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | struct B: Decodable {
// CHECK:             |        ^ error: type 'B' does not conform to protocol 'Decodable'
// CHECK: [[#LINE+1]] |   let a: Foo
// CHECK:             |       ^ note: cannot automatically synthesize 'Decodable' because 'Foo' does not conform to 'Decodable'
// CHECK: [[#LINE+2]] | }
// CHECK: Swift.Decodable:2:5
// CHECK: 1 | public protocol Decodable {
// CHECK: 2 |     init(from decoder: Decoder) throws
// CHECK:   |     ^ note: protocol requires initializer 'init(from:)' with type 'Decodable'
// CHECK: 3 | }

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:14
// CHECK: [[#LINE-1]] | // The line below is indented with tabs, not spaces.
// CHECK: [[#LINE]]   |       foo(a: 2, b: 1, a: 2)
// CHECK:             |           ++++++~~~~------
// CHECK:             |                       ^ error: argument 'a' must precede argument 'b' [remove ', a: 2' and insert 'a: 2, ']
// CHECK: [[#LINE+1]] |

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:20
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | let 👍👍👍 = {
// CHECK:    | --> error: unable to infer complex closure return type; add explicit type to disambiguate [insert ' () -> <#Result#> in ']
// CHECK: [[#LINE+1]] |   let y = 1

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:5
// CHECK: [[#LINE-2]] | // Multi-line fix-its
// CHECK: [[#LINE-1]] | foo(a: 2, b: 1,
// CHECK:             |     ++++++~~~~-
// CHECK: [[#LINE]]   |     a: 2)
// CHECK:             |     ----
// CHECK:             |     ^ error: argument 'a' must precede argument 'b' [remove ',\n    a: 2' and insert 'a: 2, ']
// CHECK: [[#LINE+1]] |


// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:5
// CHECK: [[#LINE-3]] |
// CHECK: [[#LINE-2]] | foo(b:
// CHECK:             |     ~~
// CHECK: [[#LINE-1]] |     1,
// CHECK:             |     ~-
// CHECK: [[#LINE]]   |     a:
// CHECK:             |     --
// CHECK:             |     ^ error: argument 'a' must precede argument 'b' [remove ',\n    a:\n    2' and insert 'a:\n    2, ']
// CHECK: [[#LINE+1]] |     2)
// CHECK:             |     -
// CHECK: [[#LINE+2]] |


// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:5
// CHECK: [[#LINE-3]] |
// CHECK: [[#LINE-2]] | foo(a: 2, b:
// CHECK:             |     ++++++~~
// CHECK: [[#LINE-1]] |     1,
// CHECK:             |     ~-
// CHECK: [[#LINE]]   |     a: 2)
// CHECK:             |     ----
// CHECK:             |     ^ error: argument 'a' must precede argument 'b' [remove ',\n    a: 2' and insert 'a: 2, ']
// CHECK: [[#LINE+1]] |

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:5
// CHECK: [[#LINE-1]] | func foo(a: Int, b: Int) {
// CHECK: [[#LINE]]   |   a + b
// CHECK:             |   ~   ~
// CHECK:             |     ^ warning: result of operator '+' is unused
// CHECK: [[#LINE+1]] | }

// Test snippet truncation.
// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:3
// CHECK: [[#LINE-1]] | func baz() {
// CHECK: [[#LINE]]   |   bar(a: "hello, world!")
// CHECK:             |   ^ error: no exact matches in call to global function 'bar'
// CHECK: [[#LINE+1]] | }
// CHECK:   ...
// CHECK: [[#LINE:]]  |
// CHECK: [[#LINE+1]] | func bar(a: Int) {}
// CHECK:             |      ^ note: candidate expects value of type 'Int' for parameter #1
// CHECK: [[#LINE+2]] | func bar(a: Float) {}
// CHECK:             |      ^ note: candidate expects value of type 'Float' for parameter #1
// CHECK: [[#LINE+3]] |

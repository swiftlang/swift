// RUN: not %target-swift-frontend -diagnostic-style=swift -typecheck %/s 2>&1 | %FileCheck %s --check-prefixes=CHECK,%if parser_astgen %{ASTGEN%} %else %{CPARSER%}

// REQUIRES: swift_swift_parser

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

// Test for child notes attached directly to a "primary" error/warning diagnostic
func test(a: Int) {}
func test(a: Int) {}

// Test fallback for non-ASCII characters.
// CPARSER: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:11
// CPARSER: [[#LINE-2]] |
// CPARSER: [[#LINE]]   | let abc = "👍
// CPARSER:             | `-  error: unterminated string literal
// CPARSER: [[#LINE+1]] |

// ASTGEN: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:16
// ASTGEN: [[#LINE-2]] |
// ASTGEN: [[#LINE]]   | let abc = "👍
// ASTGEN:             | `- error: expected '"' to end string literal
// ASTGEN: [[#LINE+1]] |

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:3
// CHECK: [[#LINE-1]] |
// CHECK: [[#LINE]]   | 1 + 2
// CHECK:             | `- warning: result of operator '+' is unused
// CHECK: [[#LINE+1]] |

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:11
// CHECK:  [[#LINE-1]] |
// CHECK:  [[#LINE]] | foo(b: 1, a: 2)
// CHECK:              |         `- error: argument 'a' must precede argument 'b'
// CHECK: [[#LINE+1]]  |

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diagnostics.swift:[[#LINE:]]:6
// CHECK:  [[#LINE-1]] | func test(a: Int) {}
// CHECK:              |      `- note: 'test(a:)' previously declared here
// CHECK:  [[#LINE]]   |  func test(a: Int) {}
// CHECL:  [[#LINE+1]] |       `- error: invalid redeclaration of 'test(a:)'

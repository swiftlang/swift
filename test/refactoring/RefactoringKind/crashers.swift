// rdar://36755861
func doit(_: ()->()) {}
struct S {}
func foo() {
  doit {
    let s = S()
  }
  var a: Int
}

// RUN: %refactor -source-filename %s -pos=6:5 -end-pos=6:13 | %FileCheck %s -check-prefix=CHECK1
// RUN: %refactor -source-filename %s -pos=8:1 -end-pos=8:13 | %FileCheck %s -check-prefix=CHECK1
// CHECK1: Action begins

// rdar://33972653
func test() {
  break FOO
  continue FOO
}

// RUN: %refactor -source-filename %s -pos=17:3 -end-pos=18:15 | %FileCheck %s -check-prefix=CHECK2
// CHECK2: Action begins

// rdar://36755861
func doit(_: ()->()) {}
struct S {}
func foo() {
  doit {
    let s = S()
  }
}

// RUN: %refactor -source-filename %s -pos=6:5 -end-pos=6:13 | %FileCheck %s -check-prefix=CHECK1
// CHECK1: Action begins

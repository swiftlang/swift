// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-build-swift -module-name=test -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test


enum E {
  case A
}

struct X {}

struct S {
  let i: Int
  let e: E
  let x: X
}

var gg: Int? = 27

@inline(never)
func getOffsetE() {
  gg = MemoryLayout<S>.offset(of: \.e)
}

@inline(never)
func getOffsetX() {
  gg = MemoryLayout<S>.offset(of: \.x)
}

getOffsetE()

// CHECK: Optional(0)
print(gg as Any)

gg = 27

getOffsetX()

// CHECK: Optional(0)
print(gg as Any)

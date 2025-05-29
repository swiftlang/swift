// RUN: %empty-directory(%t) 

// 1. functional test:

// RUN: %target-build-swift %s -emit-executable -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// 2. check if the generated IR looks like what we expect:

// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s -check-prefix=CHECK-IR

// REQUIRES: executable_test

protocol `Raw First` {
  associatedtype `Raw Assoc 1`: `Raw First`
  associatedtype `Raw Assoc 2`
  init()
}

struct OuterFirst<A: `Raw First`> {
  func method(_ x: A.`Raw Assoc 1`.`Raw Assoc 2`) {
    let f: (A, A.`Raw Assoc 1`, A.`Raw Assoc 1`.`Raw Assoc 1`, A.`Raw Assoc 1`.`Raw Assoc 2`) -> () = { a, b, c, d in
      print(type(of: a))
      print(type(of: b))
      print(type(of: c))
      print(type(of: d))
    }
    f(A(), A.`Raw Assoc 1`(), A.`Raw Assoc 1`.`Raw Assoc 1`(), x)
  }
}

struct `Raw.F1`: `Raw First` {
  typealias `Raw Assoc 1` = `Raw.F2`
  typealias `Raw Assoc 2` = Void
}

struct `Raw.F2`: `Raw First` {
  typealias `Raw Assoc 1` = `Raw.F1`
  typealias `Raw Assoc 2` = `Raw.F3`
}

struct `Raw.F3` {}

OuterFirst<`Raw.F1`>().method(`Raw.F3`())
// CHECK: `Raw.F1`
// CHECK: `Raw.F2`
// CHECK: `Raw.F1`
// CHECK: `Raw.F3`

// CHECK-IR: @".str.31.`Raw\C2\A0Assoc\C2\A01` `Raw\C2\A0Assoc\C2\A02`" = private constant [32 x i8] c"`Raw\C2\A0Assoc\C2\A01` `Raw\C2\A0Assoc\C2\A02`\00"

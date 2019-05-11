// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s -implicit-check-not="FAILURE"

struct GS<T> {
// CHECK: decl: struct GS<T> for 'GS'

  let a: T.Nope
  let b: T
}

let global1: GS
let global2 = GS().x
let global3 = GS<Int>(a: 1, b: 2).b

protocol P {
// CHECK: decl: protocol P for 'P' usr=s:14swift_ide_test1PP
  associatedtype T
// CHECK: decl: associatedtype T for 'T' usr=s:14swift_ide_test1PP1TQa
  func foo() -> T
}
struct SP: P {
// CHECK: decl: struct SP : P for 'SP'
  typealias TT = Self.T
// CHECK: typealias TT = <<error type>>	for 'TT' usr=s:14swift_ide_test2SPV2TTa
}

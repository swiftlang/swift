// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s -implicit-check-not="FAILURE"

struct GS<T> {
// CHECK: decl: struct GS<T> for 'GS'
// FIXME: why do we get this?
// CHECK: decl: struct GS<T> for 'T' usr=s:tV14swift_ide_test2GS1TMx

  let a: T.Nope
// CHECK: decl: let a: <<error type>>
  let b: T
// CHECK: decl: let b: T
}

let global1: GS
// CHECK: decl: let global1: <<error type>>
let global2 = GS().x
// CHECK: decl: let global2: <<error type>>
let global3 = GS<Int>(a: 1, b: 2).b
// CHECK: decl: let global3: <<error type>>

protocol P {
// FIXME: missing protocol entries?
// CHECK: decl: protocol P for 'P' usr=s:P14swift_ide_test1P
  associatedtype T
// CHECK: decl: protocol P for 'T' usr=s:P14swift_ide_test1P1T
  func foo() -> T
// CHECK: decl: func foo() -> Self.T	for 'foo' usr=s:FP14swift_ide_test1P3fooFT_wx1T
}
struct SP: P {
// CHECK: decl: struct SP : P for 'SP'
  typealias TT = Self.T
// FIXME: should be the typealias decl
// CHECK: decl: struct SP : P for 'TT' usr=s:V14swift_ide_test2SP2TT
}

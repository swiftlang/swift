// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s -implicit-check-not="FAILURE"
// REQUIRES: rdar30680565

struct GS<T> {
// CHECK: decl: struct GS<T> for 'GS'
// FIXME: why do we get this?
// CHECK: decl: struct GS<T> for 'T' usr=s:14swift_ide_test2GSVD1TMx

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
// CHECK: decl: protocol P for 'P' usr=s:14swift_ide_test1PP
  associatedtype T
// CHECK: decl: protocol P for 'T' usr=s:14swift_ide_test1PP1T
  func foo() -> T
// CHECK: decl: func foo() -> Self.T	for 'foo' usr=s:14swift_ide_test1PP3foo1TQzyF
}
struct SP: P {
// CHECK: decl: struct SP : P for 'SP'
  typealias TT = Self.T
// FIXME: should be the typealias decl
// CHECK: decl: struct SP : P for 'TT' usr=s:14swift_ide_test2SPV2TT
}

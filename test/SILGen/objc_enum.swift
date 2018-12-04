// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership > %t.out
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-%target-ptrsize %s < %t.out
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.out

// REQUIRES: objc_interop

import gizmo


// CHECK-DAG: sil shared [serializable] @$sSo16NSRuncingOptionsV{{[_0-9a-zA-Z]*}}fC
// CHECK-DAG: sil shared [serializable] @$sSo16NSRuncingOptionsV8rawValueSivg
// CHECK-DAG: sil shared [serializable] @$sSo16NSRuncingOptionsV9hashValueSivg

// Non-payload enum ctors don't need to be instantiated at all.
// NEGATIVE-NOT: sil shared [transparent] @$sSo16NSRuncingOptionsV5MinceAbBmF
// NEGATIVE-NOT: sil shared [transparent] @$sSo16NSRuncingOptionsV12QuinceSlicedAbBmF
// NEGATIVE-NOT: sil shared [transparent] @$sSo16NSRuncingOptionsV15QuinceJuliennedAbBmF
// NEGATIVE-NOT: sil shared [transparent] @$sSo16NSRuncingOptionsV11QuinceDicedAbBmF

var runcing: NSRuncingOptions = .mince

var raw = runcing.rawValue
var eq = runcing == .quinceSliced
var hash = runcing.hashValue

func testEm<E: Equatable>(_ x: E, _ y: E) {}
func hashEm<H: Hashable>(_ x: H) {}
func rawEm<R: RawRepresentable>(_ x: R) {}

testEm(NSRuncingOptions.mince, .quinceSliced)
hashEm(NSRuncingOptions.mince)
rawEm(NSRuncingOptions.mince)
rawEm(NSFungingMask.asset)

protocol Bub {}

extension NSRuncingOptions: Bub {}

// CHECK-32-DAG: integer_literal $Builtin.IntLiteral, -2147483648
// CHECK-64-DAG: integer_literal $Builtin.IntLiteral, 2147483648
_ = NSFungingMask.toTheMax

// CHECK-DAG: sil_witness_table shared [serialized] NSRuncingOptions: RawRepresentable module gizmo
// CHECK-DAG: sil_witness_table shared [serialized] NSRuncingOptions: Equatable module gizmo
// CHECK-DAG: sil_witness_table shared [serialized] NSRuncingOptions: Hashable module gizmo
// CHECK-DAG: sil_witness_table shared [serialized] NSFungingMask: RawRepresentable module gizmo

// CHECK-DAG: sil shared [transparent] [serialized] [thunk] @$sSo16NSRuncingOptionsVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW

// Extension conformances get linkage according to the protocol's accessibility, as normal.
// CHECK-DAG: sil_witness_table hidden NSRuncingOptions: Bub module objc_enum


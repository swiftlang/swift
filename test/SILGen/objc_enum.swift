// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen > %t.out
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-%target-ptrsize %s < %t.out
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.out

// REQUIRES: objc_interop

import gizmo


// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsC
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsg8rawValueSi
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsg9hashValueSi

// Non-payload enum ctors don't need to be instantiated at all.
// NEGATIVE-NOT: sil shared [transparent] @_TFOSC16NSRuncingOptions5MinceFMS_S_
// NEGATIVE-NOT: sil shared [transparent] @_TFOSC16NSRuncingOptions12QuinceSlicedFMS_S_
// NEGATIVE-NOT: sil shared [transparent] @_TFOSC16NSRuncingOptions15QuinceJuliennedFMS_S_
// NEGATIVE-NOT: sil shared [transparent] @_TFOSC16NSRuncingOptions11QuinceDicedFMS_S_

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

// CHECK-32-DAG: integer_literal $Builtin.Int2048, -2147483648
// CHECK-64-DAG: integer_literal $Builtin.Int2048, 2147483648
_ = NSFungingMask.toTheMax

// CHECK-DAG: sil_witness_table shared [fragile] NSRuncingOptions: RawRepresentable module gizmo
// CHECK-DAG: sil_witness_table shared [fragile] NSRuncingOptions: Equatable module gizmo
// CHECK-DAG: sil_witness_table shared [fragile] NSRuncingOptions: Hashable module gizmo
// CHECK-DAG: sil_witness_table shared [fragile] NSFungingMask: RawRepresentable module gizmo

// CHECK-DAG: sil shared [transparent] [thunk] @_TTWOSC16NSRuncingOptionss16RawRepresentable5gizmoFS0_C

// Extension conformances get linkage according to the protocol's accessibility, as normal.
// CHECK-DAG: sil_witness_table hidden NSRuncingOptions: Bub module objc_enum


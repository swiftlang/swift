// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen > %t.out
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-%target-ptrsize %s < %t.out
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t.out

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

var runcing: NSRuncingOptions = .Mince

var raw = runcing.rawValue
var eq = runcing == .QuinceSliced
var hash = runcing.hashValue

func testEm<E: Equatable>(x: E, _ y: E) {}
func hashEm<H: Hashable>(x: H) {}
func rawEm<R: RawRepresentable>(x: R) {}

testEm(NSRuncingOptions.Mince, .QuinceSliced)
hashEm(NSRuncingOptions.Mince)
rawEm(NSRuncingOptions.Mince)
rawEm(NSFungingMask.Asset)

protocol Bub {}

extension NSRuncingOptions: Bub {}

// CHECK-32-DAG: integer_literal $Builtin.Int2048, -2147483648
// CHECK-64-DAG: integer_literal $Builtin.Int2048, 2147483648
_ = NSFungingMask.ToTheMax

// CHECK-DAG: sil_witness_table shared NSRuncingOptions: RawRepresentable module gizmo
// CHECK-DAG: sil_witness_table shared NSRuncingOptions: Equatable module gizmo
// CHECK-DAG: sil_witness_table shared NSRuncingOptions: Hashable module gizmo
// CHECK-DAG: sil_witness_table shared NSFungingMask: RawRepresentable module gizmo

// CHECK-DAG: sil shared [transparent] [thunk] @_TTWOSC16NSRuncingOptionss16RawRepresentable5gizmoFS0_C

// Extension conformances get linkage according to the protocol's accessibility, as normal.
// CHECK-DAG: sil_witness_table hidden NSRuncingOptions: Bub module objc_enum


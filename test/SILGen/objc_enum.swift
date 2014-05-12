// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs %s -emit-silgen | FileCheck %s

import gizmo


// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptions7fromRawfMS_FSiGSqS__
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptions5toRawfS_FT_Si
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsg9hashValueSi

// CHECK-DAG: sil shared @_TF5gizmooi2eeFTOSC16NSRuncingOptionsS0__Sb

// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions5MinceFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions12QuinceSlicedFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions15QuinceJuliennedFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions11QuinceDicedFMS_S_

var runcing: NSRuncingOptions = .Mince

var raw = runcing.toRaw()
var eq = runcing == .QuinceSliced
var hash = runcing.hashValue

func testEm<E: Equatable>(x: E, y: E) {}
func hashEm<H: Hashable>(x: H) {}
func rawEm<R: RawRepresentable>(x: R) {}

testEm(NSRuncingOptions.Mince, .QuinceSliced)
hashEm(NSRuncingOptions.Mince)
rawEm(NSRuncingOptions.Mince)
rawEm(NSFungingMask.Asset)

protocol Bub {}

extension NSRuncingOptions: Bub {}

// CHECK-DAG: sil_witness_table shared NSRuncingOptions: RawRepresentable module gizmo
// CHECK-DAG: sil_witness_table shared NSRuncingOptions: Equatable module gizmo
// CHECK-DAG: sil_witness_table shared NSRuncingOptions: Hashable module gizmo
// CHECK-DAG: sil_witness_table shared NSFungingMask: RawRepresentable module gizmo

// CHECK-DAG: sil shared @_TTWOSC16NSRuncingOptionsSs16RawRepresentableFS0_7fromRawUS0__U__fMQPS0_FQS1_7RawTypeGSqS1__

// Extension conformances get public linkage as normal.
// CHECK-DAG: sil_witness_table NSRuncingOptions: Bub module objc_enum

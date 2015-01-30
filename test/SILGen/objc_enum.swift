// RUN: %target-swift-frontend -sdk %S/Inputs %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo


// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsCfMS_FT8rawValueSi_GSqS__
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsg8rawValueSi
// CHECK-DAG: sil shared @_TFOSC16NSRuncingOptionsg9hashValueSi

// CHECK-DAG: sil shared @_TF5gizmooi2eeFTOSC16NSRuncingOptionsS0__Sb

// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions5MinceFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions12QuinceSlicedFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions15QuinceJuliennedFMS_S_
// CHECK-DAG: sil shared [transparent] @_TFOSC16NSRuncingOptions11QuinceDicedFMS_S_

var runcing: NSRuncingOptions = .Mince

var raw = runcing.rawValue
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

// CHECK-DAG: sil shared @_TTWOSC16NSRuncingOptionsSs16RawRepresentable5gizmoFS0_CUS0__U__fMQPS0_FT8rawValueQS2_8RawValue_GSqS2__

// Extension conformances get linkage occording to the protocol's accessibility, as normal.
// CHECK-DAG: sil_witness_table hidden NSRuncingOptions: Bub module objc_enum


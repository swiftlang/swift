// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module -o %t %S/Inputs/objc_protocols_Bas.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir -num-threads 8 | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant [

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE:%swift.protocol\* @_T033protocol_conformance_records_objc8RuncibleMp]]
// -- nominal type descriptor
// CHECK:           @_T0SC6NSRectVMn
// -- witness table
// CHECK:           @_T0SC6NSRectV33protocol_conformance_records_objc8RuncibleACWP
// -- reserved
// CHECK:           i32 0
// CHECK:         },
extension NSRect: Runcible {
  public func runce() {}
}

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE]]
// -- class object reference + 0x03 (class object reference, TODO: indirect me)
// CHECK:           i32 add
// CHECK:           @"got.OBJC_CLASS_$_Gizmo"
// CHECK:           i32 3
// -- witness table
// CHECK:           @_T0So5GizmoC33protocol_conformance_records_objc8RuncibleACWP
// -- reserved
// CHECK:           i32 0
// CHECK:         }
extension Gizmo: Runcible {
  public func runce() {}
}

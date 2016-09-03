// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import AppKit

protocol Pointable {
  var x: Float { get set }
  var y: Float { get set }
}

extension NSPoint: Pointable {}

extension NSReferencePoint: Pointable {}

// Make sure synthesized materializeForSet and its callbacks have shared linkage
// for properties imported from Clang

// CHECK-LABEL: sil shared [transparent] [fragile] @_TFVSC7NSPointm1xSf
// CHECK-LABEL: sil shared [transparent] [fragile] @_TFVSC7NSPointm1ySf

// CHECK-LABEL: sil shared [transparent] [fragile] @_TFCSo16NSReferencePointm1xSf
// CHECK-LABEL: sil shared [transparent] [fragile] @_TFCSo16NSReferencePointm1ySf

// CHECK-LABEL: sil shared [transparent] [fragile] @_TFFCSo16NSReferencePointm1xSfU_T_
// CHECK-LABEL: sil shared [transparent] [fragile] @_TFFCSo16NSReferencePointm1ySfU_T_

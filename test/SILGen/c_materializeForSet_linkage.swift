// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

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

// CHECK-LABEL: sil shared [transparent] [fragile] @_T0SC7NSPointV1xSffm
// CHECK-LABEL: sil shared [transparent] [fragile] @_T0SC7NSPointV1ySffm

// CHECK-LABEL: sil shared @_T0So16NSReferencePointC1xSffm
// CHECK-LABEL: sil shared @_T0So16NSReferencePointC1ySffm

// CHECK-LABEL: sil shared @_T0So16NSReferencePointC1xSffmytfU_
// CHECK-LABEL: sil shared @_T0So16NSReferencePointC1ySffmytfU_

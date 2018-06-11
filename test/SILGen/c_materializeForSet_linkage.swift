// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership -enable-objc-interop | %FileCheck %s

import AppKit

protocol Pointable {
  var x: Float { get set }
  var y: Float { get set }
}

extension NSPoint: Pointable {}

extension NSReferencePoint: Pointable {}

// Make sure synthesized materializeForSet and its callbacks have shared linkage
// for properties imported from Clang

// CHECK-LABEL: sil shared [transparent] [serializable] @$SSo7NSPointV1xSfvm
// CHECK-LABEL: sil shared [transparent] [serializable] @$SSo7NSPointV1ySfvm

// CHECK-LABEL: sil shared [serializable] @$SSo16NSReferencePointC1xSfvmytfU_
// CHECK-LABEL: sil shared [serializable] @$SSo16NSReferencePointC1xSfvm

// CHECK-LABEL: sil shared [serializable] @$SSo16NSReferencePointC1ySfvmytfU_
// CHECK-LABEL: sil shared [serializable] @$SSo16NSReferencePointC1ySfvm

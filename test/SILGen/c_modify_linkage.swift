// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s

import AppKit

protocol Pointable {
  var x: Float { get set }
  var y: Float { get set }
}

extension NSPoint: Pointable {}

extension NSReferencePoint: Pointable {}

// Make sure synthesized yielding mutate accessors have shared linkage
// for properties imported from Clang.

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo7NSPointV1ySfvx

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo16NSReferencePointC1xSfvx

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo16NSReferencePointC1ySfvx

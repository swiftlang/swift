// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import AppKit

protocol Pointable {
  var x: Float { get set }
  var y: Float { get set }
}

extension NSPoint: Pointable {}
// CHECK-LABEL: sil shared @_TFFVSC7NSPointm1xSfU_FTBpRBBRS_MS__T_
// CHECK-LABEL: sil shared @_TFFVSC7NSPointm1ySfU_FTBpRBBRS_MS__T_

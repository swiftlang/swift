// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -assert-config Release %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class C : Hive {}

// CHECK-LABEL: sil private @_TTVFC34objc_implicitly_unwrapped_optional1Dc
// CHECK: bb0(%0 : $Optional<NSCoder>, %1 : $D):
// CHECK:   [[THUNK:%.*]] = function_ref @_TFC34objc_implicitly_unwrapped_optional1Dc
// CHECK:   [[REF:%.*]] = apply [[THUNK]]
// CHECK:   [[RESULT:%.*]] = enum $Optional<D>, #Optional.some!enumelt.1, [[REF]] : $D
// CHECK:   return [[RESULT]] : $Optional<D>
class D : C {
  override init(coder aCoder: NSCoder) {
    super.init(coder: aCoder)
  }
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -assert-config Release %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

class C : Hive {}

// CHECK-LABEL: sil private @_TTVFC34objc_implicitly_unwrapped_optional1DcfMS0_FT5coderCSo7NSCoder_S0_
// CHECK: bb0(%0 : $ImplicitlyUnwrappedOptional<NSCoder>, %1 : $D):
// CHECK:   [[THUNK:%.*]] = function_ref @_TFC34objc_implicitly_unwrapped_optional1DcfMS0_FT5coderCSo7NSCoder_S0_
// CHECK:   [[REF:%.*]] = apply [[THUNK]]
// CHECK:   [[RESULT:%.*]] = enum $ImplicitlyUnwrappedOptional<D>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[REF]] : $D
// CHECK:   return [[RESULT]] : $ImplicitlyUnwrappedOptional<D>
class D : C {
  override init(coder aCoder: NSCoder) {
    super.init(coder: aCoder)
  }
}

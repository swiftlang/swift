// REQUIRES: plus_one_runtime

// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -module-name inlineable_attribute_objc -Xllvm -sil-full-demangle -primary-file %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class Horse : NSObject {
  public dynamic func gallop() {}
}

// Make sure we can reference dynamic thunks and curry thunks
// from inlineable scopes

// CHECK-LABEL: sil [serialized] @$S25inlineable_attribute_objc15talkAboutAHorse1hyAA5HorseC_tF : $@convention(thin) (@owned Horse) -> () {
// CHECK: function_ref @$S25inlineable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@owned Horse) -> @owned @callee_guaranteed () -> ()
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] @$S25inlineable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@owned Horse) -> @owned @callee_guaranteed () -> ()
// CHECK:   %1 = function_ref @$S25inlineable_attribute_objc5HorseC6gallopyyFTD
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S25inlineable_attribute_objc5HorseC6gallopyyFTD : $@convention(method) (@guaranteed Horse) -> ()
// CHECK: objc_method
// CHECK: return
// CHECK: }

@_inlineable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

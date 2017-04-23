// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-full-demangle -primary-file %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class Horse : NSObject {
  public dynamic func gallop() {}
}

// Make sure we can reference dynamic thunks and curry thunks
// from inlineable scopes

// CHECK-LABEL: sil [serialized] @_T025inlineable_attribute_objc15talkAboutAHorseyAA5HorseC1h_tF : $@convention(thin) (@owned Horse) -> () {
// CHECK: function_ref @_T025inlineable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@owned Horse) -> @owned @callee_owned () -> ()
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] @_T025inlineable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@owned Horse) -> @owned @callee_owned () -> ()
// CHECK:   %1 = function_ref @_T025inlineable_attribute_objc5HorseC6gallopyyFTD
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @_T025inlineable_attribute_objc5HorseC6gallopyyFTD : $@convention(method) (@guaranteed Horse) -> ()
// CHECK: class_method [volatile]
// CHECK: return
// CHECK: }

@_inlineable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

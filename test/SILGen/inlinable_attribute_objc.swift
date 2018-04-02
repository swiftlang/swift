
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -module-name inlinable_attribute_objc -Xllvm -sil-full-demangle -primary-file %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class Horse : NSObject {
  public dynamic func gallop() {}
}

// Make sure we can reference dynamic thunks and curry thunks
// from inlinable scopes

// CHECK-LABEL: sil [serialized] @$S24inlinable_attribute_objc15talkAboutAHorse1hyAA5HorseC_tF : $@convention(thin) (@guaranteed Horse) -> () {
// CHECK: function_ref @$S24inlinable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@guaranteed Horse) -> @owned @callee_guaranteed () -> ()
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] @$S24inlinable_attribute_objc5HorseC6gallopyyFTc : $@convention(thin) (@guaranteed Horse) -> @owned @callee_guaranteed () -> ()
// CHECK:   %1 = function_ref @$S24inlinable_attribute_objc5HorseC6gallopyyFTD
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S24inlinable_attribute_objc5HorseC6gallopyyFTD : $@convention(method) (@guaranteed Horse) -> ()
// CHECK: objc_method
// CHECK: return
// CHECK: }

@inlinable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

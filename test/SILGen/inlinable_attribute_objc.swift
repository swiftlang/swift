
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -module-name inlinable_attribute_objc -Xllvm -sil-full-demangle -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class Horse : NSObject {
  @objc public dynamic func gallop() {}
  @objc public func someMethod() {}
  @objc public convenience init(saddle: ()) {
    self.init()
  }
  @objc public override init() {}
}

// @objc thunks are not serialized, since they are only referenced from
// method tables.

// CHECK-LABEL: sil [thunk] [ossa] @$s24inlinable_attribute_objc5HorseC6gallopyyFTo : $@convention(objc_method) (Horse) -> ()
// CHECK-LABEL: sil [thunk] [ossa] @$s24inlinable_attribute_objc5HorseC10someMethodyyFTo : $@convention(objc_method) (Horse) -> ()
// CHECK-LABEL: sil [thunk] [ossa] @$s24inlinable_attribute_objc5HorseC6saddleACyt_tcfcTo : $@convention(objc_method) (@owned Horse) -> @owned Horse
// CHECK-LABEL: sil [thunk] [ossa] @$s24inlinable_attribute_objc5HorseCACycfcTo : $@convention(objc_method) (@owned Horse) -> @owned Horse

// However, make sure we can reference dynamic thunks and curry thunks
// from inlinable scopes

@inlinable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

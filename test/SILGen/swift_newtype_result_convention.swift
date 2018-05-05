// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/swift_newtype_result_convention.h -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc class ThingHolder: NSObject {
  // CHECK: sil hidden [thunk] @$S{{.*}}5thing{{.*}}To : $@convention(objc_method) (ThingHolder) -> @autoreleased NSThing
  @objc let thing: NSThing = NSThing("")
}

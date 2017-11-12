// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -import-objc-header %S/Inputs/swift_newtype_result_convention.h -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc class ThingHolder: NSObject {
  // CHECK: sil hidden [thunk] @_T{{.*}}5thing{{.*}}To : $@convention(objc_method) (ThingHolder) -> @autoreleased NSThing
  @objc let thing: NSThing = NSThing("")
}

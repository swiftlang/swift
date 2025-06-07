// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop %s -emit-ir -disable-objc-attr-requires-foundation-module -use-jit | %FileCheck %s
// REQUIRES: objc_codegen

import Foundation
import objc_generics

extension NSString {
  func fn() {}
}

extension GenericClass {
  @objc func fn() {}
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} private void @runtime_registration
// CHECK-NOT: @__swift_instantiateConcreteTypeFromMangledName
// CHECK: ret void

// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-silgen -I %S/Inputs/custom-modules %s | %FileCheck %s

// REQUIRES: objc_interop

import nullability
import Foundation

// null_resettable properties.
// CHECK-LABEL: sil hidden @$s18nullability_silgen18testNullResettable{{[_0-9a-zA-Z]*}}F
func testNullResettable(_ sc: SomeClass) {
  sc.defaultedProperty = nil
  sc.defaultedProperty = "hello"
  let str: String = sc.defaultedProperty
  if sc.defaultedProperty == nil { }
}

func testFunnyProperty(_ sc: SomeClass) {
  sc.funnyProperty = "hello"
  var str: String = sc.funnyProperty
}

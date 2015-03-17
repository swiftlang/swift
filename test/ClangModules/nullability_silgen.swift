// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules %s | FileCheck %s

// REQUIRES: objc_interop

import nullability;

// null_resettable properties.
// CHECK-LABEL: sil hidden @_TF18nullability_silgen18testNullResettable
func testNullResettable(sc: SomeClass) {
  sc.defaultedProperty = nil
  sc.defaultedProperty = "hello"
  let str: String = sc.defaultedProperty
  if sc.defaultedProperty == nil { }
}

func testFunnyProperty(sc: SomeClass) {
  sc.funnyProperty = "hello"
  var str: String = sc.funnyProperty
}

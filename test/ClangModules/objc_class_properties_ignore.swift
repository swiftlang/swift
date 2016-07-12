// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import ObjCClassPropertiesIgnored

func testA() {
  A.setFloatProperty(3.1415)
  _ = A.floatProperty()
  A.floatProperty = 3.14159 // expected-error{{cannot assign to immutable expression of type '() -> Float'}}

  _ = A.doubleProperty()
}


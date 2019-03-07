// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/objc_init_generics.h %s -verify

// REQUIRES: objc_interop

// expected-no-diagnostics

import Foundation

class MyConcreteClass: MyGenericClass<NSObject> {
  // Make sure we don't complain about this "override", because MyGenericClass
  // was getting an init() that was distinct from its superclass's init() due
  // to a bug in the Clang importer.
	init() {
    super.init(value: NSObject())
	}
}


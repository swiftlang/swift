// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/objc-requires-super-obj.h %s -verify-ignore-unknown
// REQUIRES: objc_interop

// Ensure the 'objc_requires_super' attribute is imported correctly as '@requiresSuper'
// and warnings are correctly emitted.

class MyCustomViewControllerSubclass1: MyCustomViewController {
  override func customViewDidLoad() {} // expected-warning {{method override is missing 'super.customViewDidLoad()' call}}

  override init() {} // expected-warning {{method override is missing 'super.init()' call}}

  deinit {} // Okay
}

class MyCustomViewControllerSubclass2: MyCustomViewController {
  override func customViewDidLoad() { // Okay
    super.customViewDidLoad()
  }

  override init() { super.init() } // Okay
}

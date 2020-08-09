// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/objc-requires-super-obj.h %s -verify
// REQUIRES: OS=macosx

// Ensure the 'objc_requires_super' attribute is imported correctly as '@requiresSuper'
// and errors are correctly emitted and supressed.

class MyCustomViewControllerSubclass1: MyCustomViewController {
  override func customViewDidLoad() {} // expected-warning {{method override is missing 'super.customViewDidLoad()' call}}
}

class MyCustomViewControllerSubclass2: MyCustomViewController {
  override func customViewDidLoad() { // Okay
    super.customViewDidLoad()
  }
}


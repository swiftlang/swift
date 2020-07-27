// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/objc-requires-super-obj.h %s -verify
// REQUIRES: OS=macosx

// Ensure the 'objc_requires_super' attribute is imported correctly as '@requiresSuper'
// and errors are correctly emitted and supressed.

class MyCustomViewControllerSubclass1: MyCustomViewController {
  override func customViewDidLoad() {}
  // expected-error@-1 {{method override is missing 'super.customViewDidLoad()' call}}
  // expected-note@-2 {{annotate method with '@ignoresSuper' if this is intentional}} {{3-3=@ignoresSuper }}
}

class MyCustomViewControllerSubclass2: MyCustomViewController {
  override func customViewDidLoad() { // Okay
    super.customViewDidLoad()
  }
}

class MyCustomViewControllerSubclass3: MyCustomViewController {
  @ignoresSuper 
  override func customViewDidLoad() {} // Okay
}

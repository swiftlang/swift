// RUN: %target-swift-frontend -typecheck %s -verify -import-objc-header %S/Inputs/objc-requires-super-obj.h
// REQUIRES: OS=macosx

// Ensure the 'objc_requires_super' attribute is imported correctly as '@requiresSuper'
// and warnings are correctly emitted and supressed.

class MyCustomViewControllerSubclass1: MyCustomViewController {
  override func customViewDidLoad() {}
  // expected-warning@-1 {{method override is missing 'super.customViewDidLoad()' call}}
  // expected-note@-2 {{annotate method with '@ignoresSuper' if this is intentional}} {{3-3=@ignoresSuper }}
}

class MyCustomViewControllerSubclass2: MyCustomViewController {
  override func customViewDidLoad() { // Okay
    super.customViewDidLoad()
  }
}

// FIXME: Adding '@ignoresSuper' should also suppress the warning.
// TODO: Investigate why the attribute doesn't have any effect
// on overrides of Clang decls.

/*
class MyCustomViewControllerSubclass3: MyCustomViewController {
  @ignoresSuper override func customViewDidLoad() {} // Okay
} 
*/
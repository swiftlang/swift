// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/inherited-inits-other.swift -verify

// expected-no-diagnostics

// Test that we get the generic signature right (which is needed for the
// super.init to properly type-check) when it comes from another source
// file (rdar://problem/44235762).
class C: B<Int> {
  override init(foo: Int) {
    super.init(foo: foo)
  }
}

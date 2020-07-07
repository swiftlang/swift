// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name MessageFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name MessageFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/MessageFilled.symbols.json
// RUN: %FileCheck %s --input-file %t/MessageFilled.symbols.json --check-prefix=TRANSITIVE

// REQUIRES: OS=macosx

@available(macOS, deprecated, message: "S message")
public struct S {
  // A child never inherits its parent's availability message because it
  // may not make sense 100% of the time.

  // CHECK-LABEL: "precise": "s:13MessageFilled1SV3fooyyF"
  // CHECK: "availability": [
  // CHECK-NEXT:   {
  // CHECK-NEXT:     "domain": "macOS",
  // CHECK-NEXT:     "message": "foo() message",
  // CHECK-NEXT:     "isUnconditionallyDeprecated": true
  // CHECK-NEXT:   }
  @available(macOS, deprecated, message: "foo() message")
  public func foo() {}
}


@available(macOS, deprecated, message: "Outer message")
public struct Outer {
  public struct Inner {
    // TRANSITIVE-LABEL: "precise": "s:13MessageFilled5OuterV5InnerV3fooyyF"
    // TRANSITIVE: "availability": [
    // TRANSITIVE-NEXT:   {
    // TRANSITIVE-NEXT:     "domain": "macOS",
    // TRANSITIVE-NEXT:     "message": "Inner.foo() message",
    // TRANSITIVE-NEXT:     "isUnconditionallyDeprecated": true
    // TRANSITIVE-NEXT:   }
    // TRANSITIVE-NEXT: ]
    @available(macOS, deprecated, message: "Inner.foo() message")
    public func foo() {}
  }
}

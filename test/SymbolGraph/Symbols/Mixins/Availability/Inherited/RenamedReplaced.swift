// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name RenamedReplaced -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name RenamedReplaced -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/RenamedReplaced.symbols.json
// RUN: %FileCheck %s --input-file %t/RenamedReplaced.symbols.json --check-prefix=TRANSITIVE

// REQUIRES: OS=macosx

@available(macOS, deprecated, renamed: "S.bar")
public struct S {
  // A parent's `renamed` field should never replace a child's.
  // It wouldn't make sense for a parent and child to both be renamed to the same thing.

  // CHECK-LABEL: "precise": "s:15RenamedReplaced1SV3fooyyF"
  // CHECK: "availability": [
  // CHECK-NEXT:   {
  // CHECK-NEXT:     "domain": "macOS",
  // CHECK-NEXT:     "renamed": "foo.bar",
  // CHECK-NEXT:     "isUnconditionallyDeprecated": true
  // CHECK-NEXT:   }
  // CHECK-NEXT: ]
  @available(macOS, deprecated, renamed: "foo.bar")
  public func foo() {}
}


@available(macOS, deprecated, renamed: "Other")
public struct Outer {
  public struct Inner {
    // TRANSITIVE-LABEL: "precise": "s:15RenamedReplaced5OuterV5InnerV3fooyyF"
    // TRANSITIVE: "availability": [
    // TRANSITIVE-NEXT:   {
    // TRANSITIVE-NEXT:     "domain": "macOS",
    // TRANSITIVE-NEXT:     "renamed": "bar",
    // TRANSITIVE-NEXT:     "isUnconditionallyDeprecated": true
    // TRANSITIVE-NEXT:   }
    // TRANSITIVE-NEXT: ]
    @available(macOS, deprecated, renamed: "bar")
    public func foo() {}
  }
}


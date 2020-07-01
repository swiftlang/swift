// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IntroducedReplaced -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IntroducedReplaced -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IntroducedReplaced.symbols.json --check-prefix=GREATERTHAN
// RUN: %FileCheck %s --input-file %t/IntroducedReplaced.symbols.json --check-prefix=EQUAL
// RUN: %FileCheck %s --input-file %t/IntroducedReplaced.symbols.json --check-prefix=TRANSITIVEGREATERTHAN
// RUN: %FileCheck %s --input-file %t/IntroducedReplaced.symbols.json --check-prefix=TRANSITIVEEQUAL

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.14)
public struct S {
  // Not possible: declarations cannot be more available than an enclosing scope
  // @available(macOS, introduced: 10.10)
  // public func lessThan() {}

  // GREATERTHAN-LABEL: "precise": "s:18IntroducedReplaced1SV11greaterThanyyF"
  // GREATERTHAN: "availability": [
  // GREATERTHAN-NEXT:   {
  // GREATERTHAN-NEXT:     "domain": "macOS",
  // GREATERTHAN-NEXT:     "introduced": {
  // GREATERTHAN-NEXT:       "major": 10,
  // GREATERTHAN-NEXT:       "minor": 15
  // GREATERTHAN-NEXT:     }
  // GREATERTHAN-NEXT:   }
  // GREATERTHAN-NEXT: ]
  @available(macOS, introduced: 10.15)
  public func greaterThan() {}

  // EQUAL-LABEL: "precise": "s:18IntroducedReplaced1SV5equalyyF",
  // EQUAL: "availability": [
  // EQUAL-NEXT:   {
  // EQUAL-NEXT:     "domain": "macOS",
  // EQUAL-NEXT:     "introduced": {
  // EQUAL-NEXT:       "major": 10,
  // EQUAL-NEXT:       "minor": 14
  // EQUAL-NEXT:     }
  // EQUAL-NEXT:   }
  // EQUAL-NEXT: ]
  @available(macOS, introduced: 10.14)
  public func equal() {}
}

@available(macOS, introduced: 10.14)
public struct Outer {
  public struct Inner {
    // TRANSITIVEGREATERTHAN-LABEL: "precise": "s:18IntroducedReplaced5OuterV5InnerV11greaterThanyyF"
    // TRANSITIVEGREATERTHAN: "availability": [
    // TRANSITIVEGREATERTHAN-NEXT:   {
    // TRANSITIVEGREATERTHAN-NEXT:     "domain": "macOS",
    // TRANSITIVEGREATERTHAN-NEXT:     "introduced": {
    // TRANSITIVEGREATERTHAN-NEXT:       "major": 10,
    // TRANSITIVEGREATERTHAN-NEXT:       "minor": 15
    // TRANSITIVEGREATERTHAN-NEXT:     }
    // TRANSITIVEGREATERTHAN-NEXT:   }
    // TRANSITIVEGREATERTHAN-NEXT: ]
    @available(macOS, introduced: 10.15)
    public func greaterThan() {}

    // TRANSITIVEEQUAL-LABEL: precise": "s:18IntroducedReplaced5OuterV5InnerV5equalyyF"
    // TRANSITIVEEQUAL: "availability": [
    // TRANSITIVEEQUAL-NEXT:   {
    // TRANSITIVEEQUAL-NEXT:     "domain": "macOS",
    // TRANSITIVEEQUAL-NEXT:     "introduced": {
    // TRANSITIVEEQUAL-NEXT:       "major": 10,
    // TRANSITIVEEQUAL-NEXT:       "minor": 14
    // TRANSITIVEEQUAL-NEXT:     }
    // TRANSITIVEEQUAL-NEXT:   }
    // TRANSITIVEEQUAL-NEXT: ]
    @available(macOS, introduced: 10.14)
    public func equal() {}
  }
}

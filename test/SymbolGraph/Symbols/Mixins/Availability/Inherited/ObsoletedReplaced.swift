// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ObsoletedReplaced -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ObsoletedReplaced -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=LESSTHAN
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=GREATERTHAN
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=EQUAL
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=TRANSITIVELESSTHAN
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=TRANSITIVEGREATERTHAN
// RUN: %FileCheck %s --input-file %t/ObsoletedReplaced.symbols.json --check-prefix=TRANSITIVEEQUAL

// REQUIRES: OS=macosx

@available(macOS, obsoleted: 98.14)
public struct S {
  // LESSTHAN-LABEL: "precise": "s:17ObsoletedReplaced1SV8lessThanyyF",
  // LESSTHAN: "availability": [
  // LESSTHAN-NEXT:   {
  // LESSTHAN-NEXT:     "domain": "macOS",
  // LESSTHAN-NEXT:     "obsoleted": {
  // LESSTHAN-NEXT:       "major": 98,
  // LESSTHAN-NEXT:       "minor": 10
  // LESSTHAN-NEXT:     }
  // LESSTHAN-NEXT:   }
  // LESSTHAN-NEXT: ]
  @available(macOS, obsoleted: 98.10)
  public func lessThan() {}

  // GREATERTHAN-LABEL: "precise": "s:17ObsoletedReplaced1SV11greaterThanyyF",
  // GREATERTHAN: "availability": [
  // GREATERTHAN-NEXT:   {
  // GREATERTHAN-NEXT:     "domain": "macOS",
  // GREATERTHAN-NEXT:     "obsoleted": {
  // GREATERTHAN-NEXT:       "major": 98,
  // GREATERTHAN-NEXT:       "minor": 14
  // GREATERTHAN-NEXT:     }
  // GREATERTHAN-NEXT:   }
  // GREATERTHAN-NEXT: ]
  @available(macOS, obsoleted: 98.15)
  public func greaterThan() {}

  // EQUAL-LABEL: "precise": "s:17ObsoletedReplaced1SV5equalyyF",
  // EQUAL: "availability": [
  // EQUAL-NEXT:   {
  // EQUAL-NEXT:     "domain": "macOS",
  // EQUAL-NEXT:     "obsoleted": {
  // EQUAL-NEXT:       "major": 98,
  // EQUAL-NEXT:       "minor": 14
  // EQUAL-NEXT:     }
  // EQUAL-NEXT:   }
  // EQUAL-NEXT: ]
  @available(macOS, obsoleted: 98.14)
  public func equal() {}
}

@available(macOS, obsoleted: 98.14)
public struct Outer {
  public struct Inner {
    // TRANSITIVELESSTHAN-LABEL: "precise": "s:17ObsoletedReplaced5OuterV5InnerV8lessThanyyF"
    // TRANSITIVELESSTHAN: "availability": [
    // TRANSITIVELESSTHAN-NEXT:   {
    // TRANSITIVELESSTHAN-NEXT:     "domain": "macOS",
    // TRANSITIVELESSTHAN-NEXT:     "obsoleted": {
    // TRANSITIVELESSTHAN-NEXT:       "major": 98,
    // TRANSITIVELESSTHAN-NEXT:       "minor": 10
    // TRANSITIVELESSTHAN-NEXT:     }
    // TRANSITIVELESSTHAN-NEXT:   }
    // TRANSITIVELESSTHAN-NEXT: ]
    @available(macOS, obsoleted: 98.10)
    public func lessThan() {}

    // TRANSITIVEGREATERTHAN-LABEL:"precise": "s:17ObsoletedReplaced5OuterV5InnerV11greaterThanyyF"
    // TRANSITIVEGREATERTHAN: "availability": [
    // TRANSITIVEGREATERTHAN-NEXT:   {
    // TRANSITIVEGREATERTHAN-NEXT:     "domain": "macOS",
    // TRANSITIVEGREATERTHAN-NEXT:     "obsoleted": {
    // TRANSITIVEGREATERTHAN-NEXT:       "major": 98,
    // TRANSITIVEGREATERTHAN-NEXT:       "minor": 14
    // TRANSITIVEGREATERTHAN-NEXT:     }
    // TRANSITIVEGREATERTHAN-NEXT:   }
    // TRANSITIVEGREATERTHAN-NEXT: ]
    @available(macOS, obsoleted: 98.15)
    public func greaterThan() {}

    // TRANSITIVEEQUAL-LABEL:"precise": "s:17ObsoletedReplaced5OuterV5InnerV5equalyyF"
    // TRANSITIVEEQUAL: "availability": [
    // TRANSITIVEEQUAL-NEXT:   {
    // TRANSITIVEEQUAL-NEXT:     "domain": "macOS",
    // TRANSITIVEEQUAL-NEXT:     "obsoleted": {
    // TRANSITIVEEQUAL-NEXT:       "major": 98,
    // TRANSITIVEEQUAL-NEXT:       "minor": 14
    // TRANSITIVEEQUAL-NEXT:     }
    // TRANSITIVEEQUAL-NEXT:   }
    // TRANSITIVEEQUAL-NEXT: ]
    @available(macOS, obsoleted: 98.14)
    public func equal() {}
  }
}


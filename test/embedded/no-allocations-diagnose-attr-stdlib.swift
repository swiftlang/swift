// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -Werror HeapAllocation -wmo -verify
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -no-allocations -wmo -verify

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// An array literal makes the standard library's array storage class reachable, including its
// allocating initializer, which nothing calls. That allocation can't be reported at a call in this
// file, so it is not diagnosed at all; if it were, `@diagnose` could not suppress it, because it
// would not be located in the annotated function.
//
// A problem in a standard library function is reported at each call in this file that reaches it,
// so an ignored function doesn't hide the same problem in another function.

@diagnose(HeapAllocation, as: ignored, reason: "the repro's exception")
public func ignored() -> [Int] {
  [1, 2, 3]
}

public func notIgnored() -> [Int] {
  [1, 2, 3] // expected-error {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}
}

public func alsoNotIgnored() -> [Int] {
  [4, 5, 6] // expected-error {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}
}

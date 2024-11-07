// This test is checking that usage of an existential (which is normally
// disallowed in embedded Swift and flagged in IRGen) is left undiagnosed
// because the context is @_unavailableInEmbedded.
//
// The breakdown of that is
// - (1) @_unavailableInEmbedded makes the declaration unavailable,
// - (2) unavailable function bodies is removed in embedded Swift,
// - (3) the test() function is not reported by the existential checker.

// RUN: %target-swift-frontend -emit-ir %s -parse-stdlib  -wmo | %FileCheck %s --check-prefix CHECK-A
// RUN: %target-swift-frontend -emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -wmo | %FileCheck %s --check-prefix CHECK-B

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public protocol Player {}
struct Concrete: Player {}

@_unavailableInEmbedded
public func test() -> any Player {
  Concrete() // no error because we're in unavailable-in-embedded context
}

// CHECK-A: $s4main4testAA6Player_pyF
// CHECK-B-NOT: $s4main4testAA6Player_pyF

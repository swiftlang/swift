// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -Xcc -std=c++20 -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module ImmediateThrows {
  header "immediate.h"
  requires cplusplus
}

//--- immediate.h
#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))
consteval int immediateFunction(int value) SWIFT_THROWS { return value; }
struct ImmediateMethods {
  consteval int immediate(int value) const SWIFT_THROWS { return value; }
};
constexpr int constexprFunction(int value) SWIFT_THROWS { return value; }

//--- check.swift
import ImmediateThrows

func checkImmediate() throws {
  _ = immediateFunction(1) // expected-error {{'immediateFunction' is unavailable: SWIFT_THROWS is not supported on consteval functions}}
  _ = ImmediateMethods().immediate(1) // expected-error {{'immediate' is unavailable: SWIFT_THROWS is not supported on consteval functions}}
  _ = try constexprFunction(1)
  let _: (CInt) throws -> CInt = constexprFunction
}

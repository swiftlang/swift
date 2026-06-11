// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
void foo(const int *__counted_by(len) __noescape x, int len);

//--- test.swift
// The `@_SwiftifyImport`-generated `Span` overload on the imported C
// declaration must not appear in overload resolution for `foo(_:)` once the
// user supplies an `@implementation(safe)`, otherwise both compete with the
// same parameter list and produce an ambiguity. The swiftify peer is marked
// `@available(*, unavailable)` so the user's safe implementation wins.
@c @implementation(safe)
public func foo(_ x: Span<CInt>) {}

// Use the safe function with a Span argument. Resolution should pick the
// user's safe implementation (no `@available` diagnostic emitted).
public func call(_ s: Span<CInt>) {
  foo(s)
}

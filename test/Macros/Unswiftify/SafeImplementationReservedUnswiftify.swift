// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -enable-experimental-feature SafeInteropImplementations -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
void foo(const int *__counted_by(len) x, int len);

//--- test.swift
// `_Unswiftify` is a compiler-internal, never-declared macro that the compiler
// attaches and binds to itself for `@c @implementation(safe)`. A user must not
// be able to spoof it: writing `@_Unswiftify` in source used to be silently
// bound to the internal macro too, producing a *second* `@c @implementation`
// peer for the same C symbol (a duplicate-symbol link error). The compiler now
// binds only the attribute it synthesized, so a user-written `@_Unswiftify`
// falls through to normal resolution and is rejected.
@c @implementation(safe)
@_Unswiftify // expected-error{{unknown attribute '_Unswiftify'}}
public func foo(_ x: UnsafeBufferPointer<CInt>) {
}

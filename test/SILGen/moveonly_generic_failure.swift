// RUN: not --crash %target-swift-emit-silgen -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module

// REQUIRES: asserts

// This test makes sure that we do not accept using Builtin.move on address only
// types.

func addressOnlyMove<T>(t: T) -> T {
    Builtin.move(t)
}

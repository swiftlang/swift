// RUN: %target-swift-frontend %s -typecheck -verify

// https://github.com/apple/swift/issues/54359

func f(_ closure: @autoclosure () -> String...) {} // expected-error {{'@autoclosure' must not be used on variadic parameters}}
f("A") // No crash

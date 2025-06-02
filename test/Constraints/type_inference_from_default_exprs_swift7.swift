// RUN: %target-typecheck-verify-swift -swift-version 7
// REQUIRES: swift7

// https://github.com/swiftlang/swift/issues/72199
enum S72199_1 {
  func testS72199_1<T>(_: T = 42, _: [T]) {}
  // expected-error@-1 {{cannot use default expression for inference of 'T' because it is inferrable from parameters #0, #1}}
}

func testS72199<T>(_: T = 42, _: [T]) {}
// expected-error@-1 {{cannot use default expression for inference of 'T' because it is inferrable from parameters #0, #1}}

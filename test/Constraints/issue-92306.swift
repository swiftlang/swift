// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/92306

func test(
  found: UnsafeMutableRawPointer,
  start: UnsafePointer<UInt8>,
  offset: inout Int
) {
  offset += (found - start)
  // expected-error@-1 {{binary operator '-' cannot be applied}}
}

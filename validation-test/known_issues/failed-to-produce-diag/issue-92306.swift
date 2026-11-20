// rdar://187526337 (https://github.com/swiftlang/swift/issues/92306)
// RUN: %target-typecheck-verify-swift

func test(
  found: UnsafeMutableRawPointer,
  start: UnsafePointer<UInt8>,
  offset: inout Int
) {
  offset += (found - start) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

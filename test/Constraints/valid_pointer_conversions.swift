// RUN: %target-typecheck-verify-swift -swift-version 5

func foo(_ a: [[UInt8]], _ p: [UnsafeRawPointer]) {
  foo(a, a) // expect-warning {{all paths through this function will call itself}}
}

// rdar://problem/44658089
func takesPtr(_: UnsafePointer<UInt8>) {}

func givesPtr(_ str: String) {
  takesPtr(UnsafePointer(str))
  // expected-error@-1 {{initialization of 'UnsafePointer<UInt8>' results in a dangling pointer}}
  // expected-note@-2 {{implicit argument conversion from 'String' to 'UnsafePointer<UInt8>' produces a pointer valid only for the duration of the call}}
}

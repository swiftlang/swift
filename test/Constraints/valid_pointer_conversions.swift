// RUN: %target-typecheck-verify-swift

func foo(_ a: [[UInt8]], _ p: [UnsafeRawPointer]) {
  foo(a, a) // expect-warning {{all paths through this function will call itself}}
}

// rdar://problem/44658089
func takesPtr(_: UnsafePointer<UInt8>) {}

func givesPtr(_ str: String) {
  takesPtr(UnsafePointer(str)) // expected-warning {{initialization of 'UnsafePointer<UInt8>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note @-1 {{implicit argument conversion from 'String' to 'UnsafePointer<UInt8>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}
}

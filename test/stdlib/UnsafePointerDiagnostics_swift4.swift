// RUN: %target-typecheck-verify-swift -swift-version 4

// Test that we get a custom diagnostic for an ephemeral conversion to non-ephemeral param for an Unsafe[Mutable][Raw][Buffer]Pointer init.
func unsafePointerInitEphemeralConversions() {
  class C {}
  var foo = 0
  var str = ""
  var arr = [0]
  var optionalArr: [Int]? = [0]
  var c: C?

  _ = UnsafePointer(&foo) // expected-warning {{initialization of 'UnsafePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer<Int8>("") // expected-warning {{initialization of 'UnsafePointer<Int8>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer<Int8>(str) // expected-warning {{initialization of 'UnsafePointer<Int8>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer([0]) // expected-warning {{initialization of 'UnsafePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(arr) // expected-warning {{initialization of 'UnsafePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(&arr) // expected-warning {{initialization of 'UnsafePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(optionalArr) // expected-warning {{initialization of 'UnsafePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutablePointer(&foo) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutablePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(&arr) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutablePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: &foo) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer<Int8>(mutating: "") // expected-warning {{initialization of 'UnsafeMutablePointer<Int8>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer<Int8>(mutating: str) // expected-warning {{initialization of 'UnsafeMutablePointer<Int8>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: [0]) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: arr) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: &arr) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: optionalArr) // expected-warning {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeRawPointer(&foo) // expected-warning {{initialization of 'UnsafeRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(str) // expected-warning {{initialization of 'UnsafeRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(arr) // expected-warning {{initialization of 'UnsafeRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(&arr) // expected-warning {{initialization of 'UnsafeRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(optionalArr) // expected-warning {{initialization of 'UnsafeRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableRawPointer(&foo) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(&arr) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: &foo) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: str) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: arr) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: &arr) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: optionalArr) // expected-warning {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = AutoreleasingUnsafeMutablePointer(&c) // expected-warning {{initialization of 'AutoreleasingUnsafeMutablePointer<C?>' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'AutoreleasingUnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call}}


  _ = UnsafeBufferPointer(start: &foo, count: 0) // expected-warning {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer<Int8>(start: str, count: 0) // expected-warning {{initialization of 'UnsafeBufferPointer<Int8>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: arr, count: 0) // expected-warning {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: &arr, count: 0) // expected-warning {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: optionalArr, count: 0) // expected-warning {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableBufferPointer(start: &foo, count: 0) // expected-warning {{initialization of 'UnsafeMutableBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutablePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableBufferPointer(start: &arr, count: 0) // expected-warning {{initialization of 'UnsafeMutableBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutablePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeRawBufferPointer(start: &foo, count: 0) // expected-warning {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: str, count: 0) // expected-warning {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: arr, count: 0) // expected-warning {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: &arr, count: 0) // expected-warning {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: optionalArr, count: 0) // expected-warning {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableRawBufferPointer(start: &foo, count: 0) // expected-warning {{initialization of 'UnsafeMutableRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawBufferPointer(start: &arr, count: 0) // expected-warning {{initialization of 'UnsafeMutableRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}


  // FIXME: This is currently ambiguous.
  _ = OpaquePointer(&foo) // expected-error {{ambiguous use of 'init(_:)}}

  // FIXME: This is currently ambiguous.
  _ = OpaquePointer(&arr) // expected-error {{ambiguous use of 'init(_:)}}

  _ = OpaquePointer(arr) // expected-warning {{initialization of 'OpaquePointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = OpaquePointer(str) // expected-warning {{initialization of 'OpaquePointer' results in a dangling pointer; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}
}

var global = 0

// Test that we allow non-ephemeral conversions, such as inout-to-pointer for globals.
func unsafePointerInitNonEphemeralConversions() {
  _ = UnsafePointer(&global)
  _ = UnsafeMutablePointer(&global)
  _ = UnsafeRawPointer(&global)
  _ = UnsafeMutableRawPointer(&global)
  _ = UnsafeBufferPointer(start: &global, count: 0)
  _ = UnsafeMutableBufferPointer(start: &global, count: 0)
  _ = UnsafeRawBufferPointer(start: &global, count: 0)
  _ = UnsafeMutableRawBufferPointer(start: &global, count: 0)

  // FIXME: This is currently ambiguous.
  _ = OpaquePointer(&global) // expected-error {{ambiguous use of 'init(_:)'}}
}

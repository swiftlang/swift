// RUN: %target-typecheck-verify-swift

// Test that we get a custom diagnostic for an ephemeral conversion to non-ephemeral param for an Unsafe[Mutable][Raw][Buffer]Pointer init.
func unsafePointerInitEphemeralConversions() {
  class C {}
  var foo = 0
  var str = ""
  var arr = [0]
  var optionalArr: [Int]? = [0]
  var c: C?

  // FIXME(rdar://57360581): Once we re-introduce the @_nonEphemeral attribute,
  // these should produce warnings.
  _ = UnsafePointer(&foo)
  _ = UnsafePointer(&foo + 1)
  _ = UnsafePointer.init(&foo)
  _ = UnsafePointer<Int8>("")
  _ = UnsafePointer<Int8>.init("")
  _ = UnsafePointer<Int8>(str)
  _ = UnsafePointer([0])
  _ = UnsafePointer(arr)
  _ = UnsafePointer(&arr)
  _ = UnsafePointer(optionalArr)

  _ = UnsafeMutablePointer(&foo)
  _ = UnsafeMutablePointer(&arr)
  _ = UnsafeMutablePointer(&arr + 2)
  _ = UnsafeMutablePointer(mutating: &foo)
  _ = UnsafeMutablePointer<Int8>(mutating: "")
  _ = UnsafeMutablePointer<Int8>(mutating: str)
  _ = UnsafeMutablePointer(mutating: [0])
  _ = UnsafeMutablePointer(mutating: arr)
  _ = UnsafeMutablePointer(mutating: &arr)
  _ = UnsafeMutablePointer(mutating: optionalArr)

  _ = UnsafeRawPointer(&foo)
  _ = UnsafeRawPointer(str)
  _ = UnsafeRawPointer(arr)
  _ = UnsafeRawPointer(&arr)
  _ = UnsafeRawPointer(optionalArr)

  _ = UnsafeMutableRawPointer(&foo)
  _ = UnsafeMutableRawPointer(&arr)
  _ = UnsafeMutableRawPointer(mutating: &foo)
  _ = UnsafeMutableRawPointer(mutating: str)
  _ = UnsafeMutableRawPointer(mutating: arr)
  _ = UnsafeMutableRawPointer(mutating: &arr)
  _ = UnsafeMutableRawPointer(mutating: optionalArr)

  _ = UnsafeBufferPointer(start: &foo, count: 0)
  _ = UnsafeBufferPointer.init(start: &foo, count: 0)
  _ = UnsafeBufferPointer<Int8>(start: str, count: 0)
  _ = UnsafeBufferPointer<Int8>.init(start: str, count: 0)
  _ = UnsafeBufferPointer(start: arr, count: 0)
  _ = UnsafeBufferPointer(start: &arr, count: 0)
  _ = UnsafeBufferPointer(start: optionalArr, count: 0)

  _ = UnsafeMutableBufferPointer(start: &foo, count: 0)
  _ = UnsafeMutableBufferPointer(start: &arr, count: 0)

  _ = UnsafeRawBufferPointer(start: &foo, count: 0)
  _ = UnsafeRawBufferPointer(start: str, count: 0)
  _ = UnsafeRawBufferPointer(start: arr, count: 0)
  _ = UnsafeRawBufferPointer(start: &arr, count: 0)
  _ = UnsafeRawBufferPointer(start: optionalArr, count: 0)

  _ = UnsafeMutableRawBufferPointer(start: &foo, count: 0)
  _ = UnsafeMutableRawBufferPointer(start: &arr, count: 0)

  // FIXME: This is currently ambiguous.
  _ = OpaquePointer(&foo) // expected-error {{ambiguous use of 'init(_:)'}}

  // FIXME: This is currently ambiguous.
  _ = OpaquePointer(&arr) // expected-error {{ambiguous use of 'init(_:)'}}

  _ = OpaquePointer(arr)
  _ = OpaquePointer(str)
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

// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

func foo(_ a: [[UInt8]], _ p: [UnsafeRawPointer]) {
  foo(a, a) // expect-warning {{all paths through this function will call itself}}
}

// rdar://problem/44658089
func takesPtr(_: UnsafePointer<UInt8>) {}

func takesDoubleOptionalPtr(_ x: UnsafeRawPointer??) {}
func takesMutableDoubleOptionalPtr(_ x: UnsafeMutableRawPointer??) {}
func takesMutableDoubleOptionalTypedPtr(_ x: UnsafeMutablePointer<Double>??) {}

func givesPtr(_ str: String) {
  takesPtr(UnsafePointer(str)) // expected-warning {{initialization of 'UnsafePointer<UInt8>' results in a dangling pointer}}
  // expected-note @-1 {{implicit argument conversion from 'String' to 'UnsafePointer<UInt8>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}

  var i = 0
  var d = 0.0
  var arr = [1, 2, 3]

  // https://github.com/apple/swift/issues/51587
  // Allow double optional promotion for pointer conversions.

  takesDoubleOptionalPtr(&arr)
  takesDoubleOptionalPtr(arr)
  takesDoubleOptionalPtr(str)
  takesMutableDoubleOptionalPtr(&i)
  takesMutableDoubleOptionalPtr(&arr)
  takesMutableDoubleOptionalTypedPtr(&d)

  takesDoubleOptionalPtr(i) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeRawPointer?'}}
  takesMutableDoubleOptionalPtr(arr) // expected-error {{cannot convert value of type '[Int]' to expected argument type 'UnsafeMutableRawPointer?'}}

  takesMutableDoubleOptionalTypedPtr(&i) // expected-error {{cannot convert value of type 'UnsafeMutablePointer<Int>' to expected argument type 'UnsafeMutablePointer<Double>'}}
  // expected-note@-1 {{arguments to generic parameter 'Pointee' ('Int' and 'Double') are expected to be equal}}
}

// https://github.com/apple/swift/issues/54818
do {
  func f(_ x: UnsafeMutablePointer<Double>??) {}

  var i = 0
  f(&i)
  // expected-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<Int>' to expected argument type 'UnsafeMutablePointer<Double>'}}
  // expected-note@-2 {{arguments to generic parameter 'Pointee' ('Int' and 'Double') are expected to be equal}}
}

//problem/68254165 - Bad diagnostic when using String init(decodingCString:) with an incorrect pointer type
func rdar68254165(ptr: UnsafeMutablePointer<Int8>) {
  _ = String(decodingCString: ptr, as: .utf8) // expected-error {{generic parameter 'Encoding' could not be inferred}}
}

// The base of leading-dot syntax could be inferred through an implicit pointer conversion.
do {
  struct S {
    static var prop = S()
  }

  func inference_through_optional(_ ptr: UnsafePointer<S>?) {}

  inference_through_optional(&.prop) // Ok

  func inference_through_force_unwrap(name: String) {
    func test(_: UnsafeMutablePointer<Float>!) {}

    var outputs = [String: [Float]]()
    test(&outputs[name]!) // Ok
  }
}

do {
  func test<O>(_ initialValue: O, _ action: (inout O) -> Void) -> O {
  }

  func compute(_: UnsafeMutablePointer<UInt8>!) {
  }

  let result1 = test(0) { // `0` should be inferred as `UInt8`
    compute(&$0)
  }

  let result2 = test([0]) { // `0` should be inferred as `UInt8`
      compute(&$0)
  }

  let _: UInt8 = result1 // Ok
  let _: [UInt8] = result2 // Ok
}

protocol PointerProtocol {}
extension UnsafePointer: PointerProtocol {}

extension PointerProtocol {
  func foo(_ x: Self) {} // expected-note {{found this candidate}}
  func foo(_ x: UnsafePointer<CChar>) {} // expected-note {{found this candidate}}
}

func testGenericPointerConversions(
  chars: [CChar], mutablePtr: UnsafeMutablePointer<CChar>, ptr: UnsafePointer<CChar>
) {
  func id<T>(_ x: T) -> T { x }
  func optID<T>(_ x: T?) -> T { x! }
  func takesCharPtrs(_: UnsafePointer<CChar>, _: UnsafePointer<CChar>?) {}

  // Make sure we don't end up with an ambiguity here, we should prefer to
  // do the pointer conversion for `takesPtrs` not `id`.
  takesCharPtrs(chars, "a")
  takesCharPtrs(id(chars), id("a"))
  takesCharPtrs(id("a"), optID(chars))
  takesCharPtrs(mutablePtr, mutablePtr)
  takesCharPtrs(id(mutablePtr), id(mutablePtr))
  takesCharPtrs(id(mutablePtr), optID(mutablePtr))

  // Make sure this is ambiguous.
  ptr.foo(chars) // expected-error {{ambiguous use of 'foo'}}
}

// Make sure we prefer non-pack overloads when pointer conversions are involved.
func testOverloadedPackPointerConversions() {
  func takesPtr(_ ptr: UnsafePointer<CChar>) {}

  // Deprecation does not impact solution score, so we can use it to ensure
  // we don't pick it.
  @available(*, deprecated, message: "shouldn't have picked this overload")
  func packOverloaded1<each T, R>(_ xs: repeat each T, fn: (repeat each T) -> R?) {}
  func packOverloaded1<T, R>(_ x: T, fn: (T) -> R?) {}
  packOverloaded1("") { takesPtr($0) }

  @available(*, deprecated, message: "shouldn't have picked this overload")
  func packOverloaded2<each T, R>(_ xs: (repeat each T)?, fn: (repeat each T) -> R?) {}
  func packOverloaded2<T, R>(_ x: T?, fn: (T) -> R?) {}
  packOverloaded2("") { takesPtr($0) }

  @available(*, deprecated, message: "shouldn't have picked this overload")
  func packOverloaded3<each T, R>(_ xs: repeat (each T)?, fn: (repeat each T) -> R?) {}
  func packOverloaded3<T, R>(_ x: T?, fn: (T) -> R?) {}
  packOverloaded3("") { takesPtr($0) }

  @available(*, deprecated, message: "shouldn't have picked this overload")
  func packOverloaded4<each T, R>(_ xs: (repeat (each T)?)?, fn: (repeat each T) -> R?) {}
  func packOverloaded4<T, R>(_ x: T??, fn: (T) -> R?) {}
  packOverloaded4("") { takesPtr($0) }
}

// RUN: %target-typecheck-verify-swift -swift-version 5

// Test availability attributes on UnsafePointer initializers.
// Assume the original source contains no UnsafeRawPointer types.
func unsafePointerConversionAvailability(
  mrp: UnsafeMutableRawPointer,
  rp: UnsafeRawPointer,
  umpv: UnsafeMutablePointer<Void>, // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  upv: UnsafePointer<Void>, // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  umpi: UnsafeMutablePointer<Int>,
  upi: UnsafePointer<Int>,
  umps: UnsafeMutablePointer<String>,
  ups: UnsafePointer<String>) {

  let omrp: UnsafeMutableRawPointer? = mrp
  let orp: UnsafeRawPointer? = rp
  let oumpv: UnsafeMutablePointer<Void> = umpv  // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  let oupv: UnsafePointer<Void>? = upv  // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  let oumpi: UnsafeMutablePointer<Int>? = umpi
  let oupi: UnsafePointer<Int>? = upi
  let oumps: UnsafeMutablePointer<String>? = umps
  let oups: UnsafePointer<String>? = ups

  _ = UnsafeMutableRawPointer(mrp)
  _ = UnsafeMutableRawPointer(rp)   // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpv)
  _ = UnsafeMutableRawPointer(upv)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpi)
  _ = UnsafeMutableRawPointer(upi)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umps)
  _ = UnsafeMutableRawPointer(ups)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(omrp)
  _ = UnsafeMutableRawPointer(orp)   // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumpv)
  _ = UnsafeMutableRawPointer(oupv)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumpi)
  _ = UnsafeMutableRawPointer(oupi)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumps)
  _ = UnsafeMutableRawPointer(oups)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}

  // These all correctly pass with no error.
  _ = UnsafeRawPointer(mrp)
  _ = UnsafeRawPointer(rp)
  _ = UnsafeRawPointer(umpv)
  _ = UnsafeRawPointer(upv)
  _ = UnsafeRawPointer(umpi)
  _ = UnsafeRawPointer(upi)
  _ = UnsafeRawPointer(umps)
  _ = UnsafeRawPointer(ups)
  _ = UnsafeRawPointer(omrp)
  _ = UnsafeRawPointer(orp)
  _ = UnsafeRawPointer(oumpv)
  _ = UnsafeRawPointer(oupv)
  _ = UnsafeRawPointer(oumpi)
  _ = UnsafeRawPointer(oupi)
  _ = UnsafeRawPointer(oumps)
  _ = UnsafeRawPointer(oups)
  _ = UnsafePointer<Int>(upi)
  _ = UnsafePointer<Int>(oumpi)
  _ = UnsafePointer<Int>(oupi)
  _ = UnsafeMutablePointer<Int>(umpi)
  _ = UnsafeMutablePointer<Int>(oumpi)

  _ = UnsafeMutablePointer<Void>(rp) // expected-warning 4 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}} expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafeMutablePointer<Void>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umpv) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umpi) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umps) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}

  _ = UnsafePointer<Void>(rp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umpv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umpi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umps) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(ups) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}

  _ = UnsafeMutablePointer<Int>(rp) // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}} 
  _ = UnsafeMutablePointer<Int>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} 
  _ = UnsafeMutablePointer<Int>(orp) // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'RawPointer'}}
  _ = UnsafeMutablePointer<Int>(omrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer?' to expected argument type 'RawPointer'}} 

  _ = UnsafePointer<Int>(rp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(orp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(omrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer?' to expected argument type 'RawPointer'}}
}

func unsafeRawBufferPointerConversions(
  mrp: UnsafeMutableRawPointer,
  rp: UnsafeRawPointer,
  mrbp: UnsafeMutableRawBufferPointer,
  rbp: UnsafeRawBufferPointer,
  mbpi: UnsafeMutableBufferPointer<Int>,
  bpi: UnsafeBufferPointer<Int>) {

  let omrp: UnsafeMutableRawPointer? = mrp
  let orp: UnsafeRawPointer? = rp

  _ = UnsafeMutableRawBufferPointer(start: mrp, count: 1)
  _ = UnsafeRawBufferPointer(start: mrp, count: 1)
  _ = UnsafeMutableRawBufferPointer(start: rp, count: 1) // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'UnsafeMutableRawPointer?'}}
  _ = UnsafeRawBufferPointer(start: rp, count: 1)
  _ = UnsafeMutableRawBufferPointer(mrbp)
  _ = UnsafeRawBufferPointer(mrbp)
  _ = UnsafeMutableRawBufferPointer(rbp) // expected-error {{cannot invoke initializer for type 'UnsafeMutableRawBufferPointer' with an argument list of type '(UnsafeRawBufferPointer)'}} expected-note {{overloads for 'UnsafeMutableRawBufferPointer' exist with these partially matching parameter lists: (UnsafeMutableRawBufferPointer), (UnsafeMutableBufferPointer<T>)}}
  _ = UnsafeRawBufferPointer(rbp)
  _ = UnsafeMutableRawBufferPointer(mbpi)
  _ = UnsafeRawBufferPointer(mbpi)
  _ = UnsafeMutableRawBufferPointer(bpi) // expected-error {{cannot invoke initializer for type 'UnsafeMutableRawBufferPointer' with an argument list of type '(UnsafeBufferPointer<Int>)'}} expected-note {{overloads for 'UnsafeMutableRawBufferPointer' exist with these partially matching parameter lists: (UnsafeMutableRawBufferPointer), (UnsafeMutableBufferPointer<T>)}}
  _ = UnsafeRawBufferPointer(bpi)
  _ = UnsafeMutableRawBufferPointer(start: omrp, count: 1)
  _ = UnsafeRawBufferPointer(start: omrp, count: 1)
  _ = UnsafeMutableRawBufferPointer(start: orp, count: 1) // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'UnsafeMutableRawPointer?'}}
  _ = UnsafeRawBufferPointer(start: orp, count: 1)
}

// Test that we get a custom diagnostic for an ephemeral conversion to non-ephemeral param for an Unsafe[Mutable][Raw][Buffer]Pointer init.
func unsafePointerInitEphemeralConversions() {
  class C {}
  var foo = 0
  var str = ""
  var arr = [0]
  var optionalArr: [Int]? = [0]
  var c: C?

  _ = UnsafePointer(&foo) // expected-error {{initialization of 'UnsafePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer<Int8>("") // expected-error {{initialization of 'UnsafePointer<Int8>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer<Int8>(str) // expected-error {{initialization of 'UnsafePointer<Int8>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer([0]) // expected-error {{initialization of 'UnsafePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(arr) // expected-error {{initialization of 'UnsafePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(&arr) // expected-error {{initialization of 'UnsafePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafePointer(optionalArr) // expected-error {{initialization of 'UnsafePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutablePointer(&foo) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutablePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(&arr) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutablePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: &foo) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer<Int8>(mutating: "") // expected-error {{initialization of 'UnsafeMutablePointer<Int8>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer<Int8>(mutating: str) // expected-error {{initialization of 'UnsafeMutablePointer<Int8>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: [0]) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: arr) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: &arr) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutablePointer(mutating: optionalArr) // expected-error {{initialization of 'UnsafeMutablePointer<Int>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeRawPointer(&foo) // expected-error {{initialization of 'UnsafeRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(str) // expected-error {{initialization of 'UnsafeRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(arr) // expected-error {{initialization of 'UnsafeRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(&arr) // expected-error {{initialization of 'UnsafeRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawPointer(optionalArr) // expected-error {{initialization of 'UnsafeRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableRawPointer(&foo) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(&arr) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: &foo) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: str) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: arr) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: &arr) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawPointer(mutating: optionalArr) // expected-error {{initialization of 'UnsafeMutableRawPointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = AutoreleasingUnsafeMutablePointer(&c) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<C?>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'AutoreleasingUnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call}}


  _ = UnsafeBufferPointer(start: &foo, count: 0) // expected-error {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer<Int8>(start: str, count: 0) // expected-error {{initialization of 'UnsafeBufferPointer<Int8>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: arr, count: 0) // expected-error {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: &arr, count: 0) // expected-error {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeBufferPointer(start: optionalArr, count: 0) // expected-error {{initialization of 'UnsafeBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableBufferPointer(start: &foo, count: 0) // expected-error {{initialization of 'UnsafeMutableBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutablePointer<Int>?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableBufferPointer(start: &arr, count: 0) // expected-error {{initialization of 'UnsafeMutableBufferPointer<Int>' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutablePointer<Int>?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeRawBufferPointer(start: &foo, count: 0) // expected-error {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: str, count: 0) // expected-error {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: arr, count: 0) // expected-error {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: &arr, count: 0) // expected-error {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeRawBufferPointer(start: optionalArr, count: 0) // expected-error {{initialization of 'UnsafeRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]?' to 'UnsafeRawPointer?' produces a pointer valid only for the duration of the call}}


  _ = UnsafeMutableRawBufferPointer(start: &foo, count: 0) // expected-error {{initialization of 'UnsafeMutableRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = UnsafeMutableRawBufferPointer(start: &arr, count: 0) // expected-error {{initialization of 'UnsafeMutableRawBufferPointer' results in a dangling buffer pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}


  // FIXME: Currently produces ambiguity error instead of custom non-ephemeral error.
  _ = OpaquePointer(&foo) // expected-error {{cannot invoke initializer for type 'OpaquePointer' with an argument list of type '(inout Int)'}}
  // expected-note@-1 {{overloads for 'OpaquePointer' exist with these partially matching parameter lists:}}

  // FIXME: Currently produces ambiguity error instead of custom non-ephemeral error.
  _ = OpaquePointer(&arr) // expected-error {{cannot invoke initializer for type 'OpaquePointer' with an argument list of type '(inout [Int])'}}
  // expected-note@-1 {{overloads for 'OpaquePointer' exist with these partially matching parameter lists:}}

  _ = OpaquePointer(arr) // expected-error {{initialization of 'OpaquePointer' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  _ = OpaquePointer(str) // expected-error {{initialization of 'OpaquePointer' results in a dangling pointer}}
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

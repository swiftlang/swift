// RUN: %target-typecheck-verify-swift -swift-version 4

var str = ""
var arr: [Int8] = [5]
var value: Int8 = 5

func foo(_ x: @_nonEphemeral UnsafeMutableRawPointer, _ y: Int) {}
func bar(_ x: @_nonEphemeral UnsafePointer<Int8>, _ y: Int) {}
func baz(_ x: @_nonEphemeral UnsafeRawPointer) {}
func qux(_ x: @_nonEphemeral UnsafeMutablePointer<Int8>) {}

foo(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

bar(str, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

bar(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

baz(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

qux(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

// These are all okay, `value` is a top-level variable and therefore gets a static pointer value.
foo(&value, 5)
foo(&str, 5)
baz(&value)
baz(&str)
qux(&value)

struct S {
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

class C {
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

var globalC = C()
var globalS = S()
var globalTupleOfS = (S(), S())
var globalOptOfS: S?
var globalWithObservers: Int8 = 0 { didSet {} }

// These are all okay, we can get stable pointer values through force unwraps and stored field accesses on global structure and tuple variables.
foo(&globalS, 5)
baz(&globalC)
baz(&globalS.storedProperty)
baz(&globalTupleOfS.0)
baz(&globalTupleOfS.0.storedProperty)
baz(&globalOptOfS!)
baz(&globalOptOfS!.storedProperty)

// But we cannot do the same for class bases, properties with observers, computed properties or subscripts.

foo(&globalC.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalC.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalC.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalC[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalS[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalS.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalTupleOfS.0.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalTupleOfS.0.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalOptOfS!.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalOptOfS!.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

baz(&globalWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

// The following are not okay as they're local.
func testInoutToPointerOfLocal() {
  var local: Int8 = 0

  foo(&local, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  baz(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  qux(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}
}

// Check that @_non_ephemeral is preserved through type inference.
let f1 = foo
f1(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

let f2 = bar
f2(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

let f3 = baz
f3(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

let f4 = qux
f4(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

struct S1 {
  static func foo(_ x: String = "", ptr: @_nonEphemeral UnsafeMutableRawPointer) {}
  func bar(ptr: @_nonEphemeral UnsafeMutableRawPointer = UnsafeMutableRawPointer(&globalS)) {}
}

func testNonEphemeralInMethods() {
  var local = 0

  S1.foo(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  S1.foo("", ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  let s1 = S1()
  s1.bar() // okay.
  s1.bar(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

infix operator ^^^
func ^^^ (lhs: @_nonEphemeral UnsafeMutableRawPointer, rhs: Int) {}

func testNonEphemeralInOperators() {
  var local = 0

  &local ^^^ 1 // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

func testNonEphemeralInClosures() {
  var local = 0

  let fn: (@_nonEphemeral UnsafeMutableRawPointer) -> Void = { _ in }
  fn(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

struct S2 {
  var ptr1: UnsafeMutableRawPointer
  lazy var ptr2 = UnsafeMutableRawPointer(&globalS)
}

func testNonEphemeralInMemberwiseInit() {
  var local = 0

  _ = S2(ptr1: &globalS, ptr2: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = S2(ptr1: &local, ptr2: &globalS) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

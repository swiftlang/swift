// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

protocol TestProtocol1 {}

// Check that the generic parameter is called 'Memory'.
extension Optional where Wrapped: TestProtocol1 {
  var _wrappedIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension ImplicitlyUnwrappedOptional where Wrapped: TestProtocol1 {
  var _wrappedIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var x: Optional<Int> = nil
if x != nil { 
  print("x is non-empty!")
} else { 
  print("an empty optional is logically false")
}
// CHECK: an empty optional is logically false

switch x {
case .Some(let y):
  assert(false, "Something's wrong here!")
case .None:
  ()
}

x = .Some(0)

x = .Some(1)

if x != nil {
  print("a non-empty optional is logically true") 
} else { 
  assert(false, "x is empty!")
}
// CHECK: a non-empty optional is logically true

if x == nil { 
  print("logical negation fails 0")
} else { 
  print("logical negation works 0") 
}
// CHECK: logical negation works 0

if true {
  var y1: Optional<Int> = .None
  if y1 == nil {
    print("y1 is .None")
  }
  // CHECK: y1 is .None

  var y2: Optional<Int> = .None
  if y2 == nil {
    print("y2 is .None")
  }
  // CHECK: y2 is .None
}

func optional_param(x: Optional<Int>) {
  if x == nil {
    print("optional param OK")
  }
}
optional_param(.None)
// CHECK: optional param OK

func optional_return() -> Optional<Int> {
  return .None
}
if optional_return() == nil {
  print("optional return OK")
}
// CHECK: optional return OK

switch x {
case .Some(let y):
  print("destructuring bind: \(y).")
case .None:
  ()
}
// CHECK: destructuring bind: 1.


print("forced extraction: \(x!).")
// CHECK: forced extraction: 1.

print("forced extraction use: \(x!.successor()).")
// CHECK-NEXT: forced extraction use: 2.

func testRelation(p: (Int?, Int?) -> Bool) {
  typealias optPair = (Int?, Int?)
  
  let relationships: [optPair] = [
    (1, 1), (1, 2), (2, 1), (1, .None), (.None, 1), (.None, .None)
  ]

  var prefix = ""
  for (l,r) in relationships {
    print("\(prefix)\(p(l, r))", terminator: "")
    prefix=", "
  }
  print(".")
}

testRelation(==)
// CHECK-NEXT: true, false, false, false, false, true.

testRelation(!=)
// CHECK-NEXT: false, true, true, true, true, false

testRelation(<)
// CHECK-NEXT: false, true, false, false, true, false.

import StdlibUnittest
import Swift

let OptionalTests = TestSuite("Optional")

struct X {}
class C {}

class E: Equatable {}
func == (_: E, _: E) -> Bool { return true }

OptionalTests.test("nil comparison") {
  let v0: Int? = nil
  let v1: Int? = 1

  expectFalse(v1 == nil)
  expectTrue(v1 != nil)
  expectTrue(v0 == nil)
  expectFalse(v0 != nil)

  expectFalse(nil == v1)
  expectTrue(nil != v1)
  expectTrue(nil == v0)
  expectFalse(nil != v0)

  
  let e0: E? = nil
  let e1: E? = E()
  
  expectFalse(e1 == nil)
  expectTrue(e1 != nil)
  expectTrue(e0 == nil)
  expectFalse(e0 != nil)

  expectFalse(nil == e1)
  expectTrue(nil != e1)
  expectTrue(nil == e0)
  expectFalse(nil != e0)

  /*
  // FIXME: <rdar://problem/17489239> Optional<T>() == nil where T: !Equatable
  let _: X? = nil
  let _: X? = X()

  expectFalse(x1 == nil)
  expectTrue(x1 != nil)
  expectTrue(x0 == nil)
  expectFalse(x0 != nil)

  expectFalse(nil == x1)
  expectTrue(nil != x1)
  expectTrue(nil == x0)
  expectFalse(nil != x0)
  */
}

OptionalTests.test("??") {
  var counter = 0
  func nextCounter() -> Int { return counter++ }
  func nextCounter2() -> Int? { return nextCounter() }

  let a: Int? = 123
  let b: Int? = nil
  let c: Int? = nil
  let d: Int? = 456
  let e: Int? = nil
  let f: Int? = nil

  expectEqual(a ?? nextCounter(), 123)
  expectEqual(b ?? nextCounter(), 0)
  expectEqual(c ?? nextCounter(), 1)
  expectEqual(d ?? nextCounter(), 456)
  expectEqual(e ?? d ?? nextCounter(), 456)
  expectEqual(f ?? nextCounter(), 2)

  expectEqual(c ?? d, Optional(456))
  expectEqual(c ?? e, nil)
  expectEqual(a ?? nextCounter2(), Optional(123))
  expectEqual(b ?? nextCounter2(), Optional(3))
  expectEqual(c ?? nextCounter2(), Optional(4))
  expectEqual(d ?? nextCounter2(), Optional(456))
  expectEqual(e ?? d ?? nextCounter2(), Optional(456))
  expectEqual(f ?? nextCounter2(), Optional(5))
}

OptionalTests.test("flatMap") {
  let half: Int32 -> Int16? =
    { if $0 % 2 == 0 { return Int16($0 / 2) } else { return .None } }

  expectOptionalEqual(2 as Int16, half(4))
  expectEmpty(half(3))

  expectEmpty((.None as Int32?).flatMap(half))
  expectOptionalEqual(2 as Int16, (4 as Int32?).flatMap(half))
  expectEmpty((3 as Int32?).flatMap(half))
}

@inline(never)
func anyToAny<T, U>(a: T, _: U.Type) -> U {
  return a as! U
}
@inline(never)
func anyToAnyOrNil<T, U>(a: T, _: U.Type) -> U? {
  return a as? U
}
func canGenericCast<T, U>(a: T, _ ty: U.Type) -> Bool {
  return anyToAnyOrNil(a, ty) != nil
}

OptionalTests.test("Casting Optional") {
  let x = C()
  let sx: C? = x
  let nx: C? = nil
  expectTrue(anyToAny(x, Optional<C>.self)! === x)
  expectTrue(anyToAny(sx, C.self) === x)
  expectTrue(anyToAny(sx, Optional<C>.self)! === x)

  expectTrue(anyToAny(nx, Optional<C>.self) == nil)
  expectTrue(anyToAnyOrNil(nx, C.self) == nil)

  let i = Int.max
  let si: Int? = Int.max
  let ni: Int? = nil
  expectEqual(anyToAny(i, Optional<Int>.self)!, Int.max)
  expectEqual(anyToAny(si, Int.self), Int.max)
  expectEqual(anyToAny(si, Optional<Int>.self)!, Int.max)

  expectTrue(anyToAny(ni, Optional<Int>.self) == nil)
  expectTrue(anyToAnyOrNil(ni, Int.self) == nil)

  let ssx: C?? = sx
  expectTrue(anyToAny(ssx, Optional<C>.self)! === x)
  expectTrue(anyToAny(x, Optional<Optional<C>>.self)!! === x)
  expectTrue(anyToAnyOrNil(ni, Int.self) == nil)
}

OptionalTests.test("Casting Optional Traps") {
  let nx: C? = nil
  expectCrashLater()
  anyToAny(nx, Int.self)
}

class TestNoString {}
class TestString: CustomStringConvertible, CustomDebugStringConvertible {
  var description: String {
    return "AString"
  }
  var debugDescription: String {
    return "XString"
  }
}
class TestStream: Streamable {
  func writeTo<Target: OutputStreamType>(inout target: Target) {
    target.write("AStream")
  }
}

func debugPrintStr<T>(a: T) -> String {
  var s = ""
  debugPrint(a, terminator: "", toStream: &s)
  return s
}
// Optional should not conform to output stream protocols itself, but is
// convertible to them if its wrapped type is.
// Furthermore, printing an Optional should always print the debug
// description regardless of whether the wrapper type conforms to an
// output stream protocol.
OptionalTests.test("Optional OutputStream") {
  let optNoString: TestNoString? = TestNoString()
  expectFalse(optNoString is CustomStringConvertible)
  expectFalse(canGenericCast(optNoString, CustomStringConvertible.self))
  expectFalse(optNoString is Streamable)
  expectFalse(canGenericCast(optNoString, Streamable.self))
  expectTrue(optNoString is CustomDebugStringConvertible)
  expectTrue(canGenericCast(optNoString, CustomDebugStringConvertible.self))
  expectEqual(String(optNoString), "Optional(main.TestNoString)")
  expectEqual(debugPrintStr(optNoString), "Optional(main.TestNoString)")

  let iouNoString: TestNoString! = TestNoString()
  // IOU directly conforms to CustomStringConvertible.
  // Disabled pending SR-164
  //   expectTrue(iouNoString is CustomStringConvertible)
  expectTrue(canGenericCast(iouNoString, CustomStringConvertible.self))
  expectFalse(iouNoString is Streamable)
  expectFalse(canGenericCast(iouNoString, Streamable.self))
  // CustomDebugStringConvertible conformance is a temporary hack.
  // Disabled pending SR-164
  //   expectTrue(iouNoString is CustomDebugStringConvertible)
  expectTrue(canGenericCast(iouNoString, CustomDebugStringConvertible.self))
  expectEqual(String(iouNoString), "main.TestNoString")
  expectEqual(debugPrintStr(iouNoString), "main.TestNoString")

  let optString: TestString? = TestString()
  expectTrue(optString is CustomStringConvertible)
  expectTrue(canGenericCast(optString, CustomStringConvertible.self))
  expectTrue(optString is CustomDebugStringConvertible)
  expectTrue(canGenericCast(optString, CustomDebugStringConvertible.self))
  expectEqual(String(TestString()), "AString")
  expectEqual(String(optString), "Optional(XString)")
  expectEqual(debugPrintStr(optString), "Optional(XString)")

  let iouString: TestString! = TestString()
  expectTrue(iouString is CustomStringConvertible)
  expectTrue(canGenericCast(iouString, CustomStringConvertible.self))
  // CustomDebugStringConvertible conformance is a temporary hack.
  expectTrue(iouString is CustomDebugStringConvertible)
  expectTrue(canGenericCast(iouString, CustomDebugStringConvertible.self))
  expectEqual(String(iouString), "AString")
  // FIXME: Ideally the debug output would be "XString", but a reasonable
  // implemention of that behavior requires conditional conformance.
  // (directly invoking debugPrint(Any) already works correctly).
  expectEqual(debugPrintStr(iouString), "AString")

  let optStream: TestStream? = TestStream()
  expectTrue(optStream is Streamable)
  expectTrue(canGenericCast(optStream, Streamable.self))
  expectTrue(optStream is CustomDebugStringConvertible)
  expectTrue(canGenericCast(optStream, CustomDebugStringConvertible.self))
  expectEqual(String(TestStream()), "AStream")
  expectEqual(String(optStream), "Optional(AStream)")
  expectEqual(debugPrintStr(optStream), "Optional(AStream)")
}

runAllTests()

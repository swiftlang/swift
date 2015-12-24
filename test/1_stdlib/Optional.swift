// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

let OptionalTests = TestSuite("Optional")

protocol TestProtocol1 {}

// Check that the generic parameter is called 'Memory'.
extension Optional where Wrapped : TestProtocol1 {
  var _wrappedIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension ImplicitlyUnwrappedOptional where Wrapped : TestProtocol1 {
  var _wrappedIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

OptionalTests.test("nil comparison") {
  var x: Int? = nil
  expectFalse(x != nil)

  switch x {
  case .Some(let y): expectUnreachable()
  case .None: break
  }

  x = .Some(1)
  expectTrue(x != nil)

  if true {
    var y1: Int? = .None
    expectTrue(y1 == nil)

    var y2: Int? = .None
    expectTrue(y2 == nil)
  }

  let x1: Int? = nil
  let x2: Int? = .None

  expectTrue(x1 == nil)
  expectTrue(x2 == nil)

  switch x {
    case .Some(let y): expectEqual("1", "\(y)")
    case .None: assert(false)
  }

  expectEqual("forced extraction: 1.", "forced extraction: \(x!).")
  expectEqual("forced extraction use: 2.", "forced extraction use: \(x!.successor()).")
}

func testRelation(p: (Int?, Int?) -> Bool) -> [Bool] {
  typealias optPair = (Int?, Int?)
  
  let relationships: [optPair] = [
    (1, 1), (1, 2), (2, 1), (1, .None), (.None, 1), (.None, .None)
  ]

  return relationships.map { p($0, $1) }
}

OptionalTests.test("Equatable") {
  expectEqual([true, false, false, false, false, true], testRelation(==))
  expectEqual([false, true, true, true, true, false], testRelation(!=))
  expectEqual([false, true, false, false, true, false], testRelation(<))
}

struct X {}
class C {}

class E : Equatable {}
func == (_: E, _: E) -> Bool { return true }

OptionalTests.test("initializers") {
  let _: X? = nil
  let _: X? = X()

  let _: C? = nil
  let _: C? = C()
}

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
  func nextCounter() -> Int { counter += 1; return counter-1 }
  func nextCounter2() -> Int? { return nextCounter() }

  let a: Int? = 123
  let b: Int? = nil
  let c: Int? = nil
  let d: Int? = 456
  let e: Int? = nil
  let f: Int? = nil

  expectEqual(123, a ?? nextCounter())
  expectEqual(0, b ?? nextCounter())
  expectEqual(1, c ?? nextCounter())
  expectEqual(456, d ?? nextCounter())
  expectEqual(456, e ?? d ?? nextCounter())
  expectEqual(2, f ?? nextCounter())

  expectEqual(Optional(456), c ?? d)
  expectEqual(nil, c ?? e)
  expectEqual(Optional(123), a ?? nextCounter2())
  expectEqual(Optional(3), b ?? nextCounter2())
  expectEqual(Optional(4), c ?? nextCounter2())
  expectEqual(Optional(456), d ?? nextCounter2())
  expectEqual(Optional(456), e ?? d ?? nextCounter2())
  expectEqual(Optional(5), f ?? nextCounter2())
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
func anyToAny<T, U>(a: T, _ : U.Type) -> U {
  return a as! U
}
@inline(never)
func anyToAnyOrNil<T, U>(a: T, _ : U.Type) -> U? {
  return a as? U
}
func canGenericCast<T, U>(a: T, _ ty : U.Type) -> Bool {
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
class TestString : CustomStringConvertible, CustomDebugStringConvertible {
  var description: String {
    return "AString"
  }
  var debugDescription: String {
    return "XString"
  }
}
class TestStream : Streamable {
  func writeTo<Target : OutputStreamType>(inout target: Target) {
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
  // IUO directly conforms to CustomStringConvertible.
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
  // implementation of that behavior requires conditional conformance.
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

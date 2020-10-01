// Casts.swift - Tests for conversion between types.
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// Contains tests for non-trapping type conversions reported by users.
///
// -----------------------------------------------------------------------------
// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -swift-version 5 -g -Onone -Xfrontend -enable-experimental-concurrency -module-name a %s -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -swift-version 5 -g -O -Xfrontend -enable-experimental-concurrency -module-name a %s -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
//
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

func blackhole<T>(_ t: T) { }

private func runtimeCast<T,U>(_ from: T, to: U.Type) -> U? {
  return from as? U
}

let CastsTests = TestSuite("Casts")

// Test for SR-426: missing release for some types after failed conversion
CastsTests.test("No leak for failed tuple casts") {
    let t: Any = (1, LifetimeTracked(0))
    expectFalse(t is Any.Type)
}

protocol P {}
class ErrClass : Error { }

CastsTests.test("No overrelease of existential boxes in failed casts") {
    // Test for crash from SR-392
    // We fail casts of an existential box repeatedly
    // to ensure it does not get over-released.
    func bar<T>(_ t: T) {
        for _ in 0..<10 {
            if case let a as P = t {
                _ = a
            }
        }
    }

    let err: Error = ErrClass()
    bar(err)
}

extension Int : P {}

// Test for SR-7664: Inconsistent optional casting behaviour with generics
// Runtime failed to unwrap multiple levels of Optional when casting.
CastsTests.test("Multi-level optionals can be casted") {
  func testSuccess<From, To>(_ x: From, from: From.Type, to: To.Type) {
    expectNotNil(x as? To)
  }
  func testFailure<From, To>(_ x: From, from: From.Type, to: To.Type) {
    expectNil(x as? To)
  }
  testSuccess(42, from: Int?.self, to: Int.self)
  testSuccess(42, from: Int??.self, to: Int.self)
  testSuccess(42, from: Int???.self, to: Int.self)
  testSuccess(42, from: Int???.self, to: Int?.self)
  testSuccess(42, from: Int???.self, to: Int??.self)
  testSuccess(42, from: Int???.self, to: Int???.self)
  testFailure(42, from: Int?.self, to: String.self)
  testFailure(42, from: Int??.self, to: String.self)
  testFailure(42, from: Int???.self, to: String.self)
}

// Test for SR-9837: Optional<T>.none not casting to Optional<U>.none in generic context
CastsTests.test("Optional<T>.none can be casted to Optional<U>.none in generic context") {
  func test<T>(_ type: T.Type) -> T? {
    return Any?.none as? T
  }

  expectEqual(type(of: test(Bool.self)), Bool?.self)
  expectEqual(type(of: test(Bool?.self)), Bool??.self)
}

// Test for SR-3871: Cannot cast from ObjC existential without going through AnyObject
#if _runtime(_ObjC)
protocol P2 {}
CastsTests.test("Cast from ObjC existential to Protocol (SR-3871)") {
  if #available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
    struct S: P2 {}

    class ObjCWrapper {
      @objc dynamic let any: Any = S()
      init() {}
    }
    let a = ObjCWrapper().any
    expectTrue(a is P2)
    // In SR-3871, the following cast failed (everything else here succeeded)
    expectNotNil(a as? P2)
    expectNotNil(a as? S)
    let b = a as AnyObject
    expectTrue(a is P2)
    expectNotNil(b as? P2)
    expectNotNil(b as? S)
  }
}
#endif

protocol P3 {}
CastsTests.test("Cast from Swift existential to Protocol") {
  struct S: P3 {}
  class SwiftWrapper {
    let any: Any = S()
    init() {}
  }
  let a = SwiftWrapper().any
  expectTrue(a is P3)
  expectNotNil(a as? P3)
  expectNotNil(a as? S)
  let b = a as AnyObject
  expectTrue(b is P3)
  expectNotNil(b as? P3)
  expectNotNil(b as? S)
}


#if _runtime(_ObjC)
extension CFBitVector : P {
  static func makeImmutable(from values: Array<UInt8>) -> CFBitVector {
    return CFBitVectorCreate(/*allocator:*/ nil, values, values.count * 8)
  }
}

extension CFMutableBitVector {
  static func makeMutable(from values: Array<UInt8>) -> CFMutableBitVector {
    return CFBitVectorCreateMutableCopy(
      /*allocator:*/ nil,
      /*capacity:*/ 0,
      CFBitVector.makeImmutable(from: values))
  }
}

func isP<T>(_ t: T) -> Bool {
  return t is P
}

CastsTests.test("Dynamic casts of CF types to protocol existentials (SR-2289)")
.skip(.custom({
      !_isDebugAssertConfiguration()
    },
    reason: "This test behaves unpredictably in optimized mode."))
.code {
  expectTrue(isP(10 as Int))
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectTrue(isP(CFBitVector.makeImmutable(from: [10, 20])))
    expectTrue(isP(CFMutableBitVector.makeMutable(from: [10, 20])))
  }
}
#endif

// Another test for SR-3871, SR-5590, SR-6309, SR-8651:
// user type in a _SwiftValue in an Optional<Any> can't be cast to a protocol.
// Note: This uses the (misnamed) _bridgeAnythingToObjectiveC so it can
// test these paths on Linux as well.
protocol P6309 {}
CastsTests.test("Casting struct -> Obj-C -> Protocol fails (SR-3871, SR-5590, SR-6309, SR-8651)") {
  struct S: P6309 {
    let value: Int
    let tracker = LifetimeTracked(13)
  }

  let a: P6309 = S(value: 13)

  let b = _bridgeAnythingToObjectiveC(a)
  let d = b as? Any
  let e = d as? P6309
  expectNotNil(e)
}


protocol P4552 {}
if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
CastsTests.test("Casting Any(Optional(T)) -> Protocol fails (SR-4552)") {
  struct S: P4552 {
    let tracker = LifetimeTracked(13)
  }

  let a = S()
  let b: S? = a
  let c = b as? Any
  let d = c as? P4552
  expectNotNil(d)
}
}

// rdar://27108240 (Optional casting bug (crash))
protocol Key {
  associatedtype Value
}
CastsTests.test("Cast to associated type") {
  // Helper function to bypass compiler cast optimizations
  func runtimeCast<From, To> (_ x: From, to: To.Type) -> To? {
    return x as? To
  }
  struct StringKey : Key {
    typealias Value = String?
  }
  var string: String?
  func value<K: Key>(forKey key: K.Type) {
    let b = runtimeCast(string, to: K.Value.self)
    expectNotNil(b)
    let c = string as? K.Value
    expectNotNil(c)
  }
  value(forKey: StringKey.self)
}

#if _runtime(_ObjC)
// rdar://36288786 (Swift metatype stored in an Objective-C id property can't be typecast back to its original type)
CastsTests.test("Store Swift metatype in ObjC property and cast back to Any.Type") {
  class MyObj {
    var sVar: Any? = nil
    @objc dynamic var objcVar: Any? = nil
  }

  let a = MyObj()

  // Double values
  a.sVar = 1.234
  a.objcVar = 1.234

  let sValue1 = a.sVar as? Double
  let objcValue1 = a.objcVar as? Double
  expectEqual(sValue1, objcValue1)

  // Swift types
  let b = Bool.self
  a.sVar = b
  a.objcVar = b

  let sValue2 = a.sVar as? Any.Type
  let objcValue2 = a.objcVar as? Any.Type
  expectTrue(sValue2 == b)
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectTrue(sValue2 == objcValue2)
    expectTrue(objcValue2 == b)
  }
}
#endif

// rdar://37793004 ([dynamic casting] [SR-7049]: Enums don't cast back from AnyHashable)
CastsTests.test("Enums don't cast back from AnyHashable (SR-7049)") {
  enum E {
    case a
  }

  // This works as expected.
  let str: AnyHashable = "hello"
  expectNotNil(str as? String) // Optional("hello")
  expectNotNil(str as? String as Any) // Optional("hello")

  // This doesn't.
  let ea: AnyHashable = E.a
  expectNotNil(ea as? E)
  expectNotNil(ea as? E as Any)
  expectEqual((ea as? E), E.a)
}

#if _runtime(_ObjC)
//rdar://39415812 ([dynamic casting] [SR-7432]: Can't see through boxed _SwiftValue when casting from @objc Type)
@objc(Exporter)
protocol Exporter: NSObjectProtocol {
  var type: Any { get }
  func export(item: Any) -> String?
}
CastsTests.test("Casts from @objc Type") {
  struct User { var name: String }

  final class UserExporter: NSObject, Exporter {
    var type: Any { return User.self }
    func export(item: Any) -> String? {
      let u = item as? User
      return u?.name
    }
  }

  let user = User(name: "Kermit")
  let exporter: Exporter = UserExporter()

  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectTrue(exporter.type is User.Type)
  }
  expectNotNil(exporter.export(item: user))
}
#endif

#if _runtime(_ObjC)
// rdar://44467533 (Swift main branch: conditional casts for _ObjectiveCBridgeable miscompile in swift-corelibs-foundation)
CastsTests.test("Conditional NSNumber -> Bool casts") {
  let x = NSNumber(value: -1) as? Bool
  expectNil(x)
}
#endif

// rdar://45217461 ([dynamic casting] [SR-8964]: Type check operator (is) fails for Any! variable holding an Error (struct) value)
if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
CastsTests.test("Casts from Any(struct) to Error (SR-8964)") {
  struct MyError: Error { }

  let a: Any! = MyError()
  let b: Any = a
  expectTrue(b is Error)
}
}

#if _runtime(_ObjC)
// rdar://15494623 (Handle dynamic cast to archetype bound to ObjC existential)
CastsTests.test("Dynamic cast to ObjC protocol") {
  func genericCast<T>(x: NSObject, _: T.Type) -> T? {
    return x as? T
  }

  let n: NSNumber = 1
  let copying = genericCast(x: n, NSCopying.self)
  expectNotNil(copying)
}
#endif

// SR-6126
if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
CastsTests.test("Nil handling for Optionals and Arrays (SR-6126)") {
  func check(_ arg: Int??) -> String {
    switch arg {
    case .none:
      return ".none"
    case .some(.none):
      return ".some(.none)"
    case .some(.some):
      return ".some(.some)"
    }
  }

  let x: Int? = .none
  let y: [Int?] = [.none]

  let a = x as Int??
  let b = (x as? Int??)!
  let b2 = runtimeCast(x, to: Int??.self)!
  let c = Int?.none as Int??
  let d = (Int?.none as? Int??)!
  let d2 = runtimeCast(Int?.none, to: Int??.self)!
  let e = (y as [Int??]).first!
  let f = (y as? [Int??])!.first!
  let f2 = runtimeCast(y, to: [Int??].self)!.first!
  let g = ([Int?.none] as [Int??]).first!
  let h = ([Int?.none] as? [Int??])!.first!
  let h2 = runtimeCast([Int?.none], to: [Int??].self)!.first!

  // Original reporter believes all of these should be .some(.none)
  expectEqual(".some(.none)", check(a)) // Xcode 9.0: .some(.none)
  expectEqual(".some(.none)", check(b)) // Xcode 9.0: .some(.none)
  expectEqual(".some(.none)", check(b2))
  expectEqual(".some(.none)", check(c)) // Xcode 9.0: .some(.none)
  expectEqual(".some(.none)", check(d)) // Xcode 9.0: .some(.none)
  expectEqual(".some(.none)", check(d2))
  expectEqual(".some(.none)", check(e)) // Xcode 9.0: .none
  expectEqual(".some(.none)", check(f)) // Xcode 9.0: .none
  expectEqual(".some(.none)", check(f2))
  expectEqual(".some(.none)", check(g)) // Xcode 9.0: .some(.none)
  expectEqual(".some(.none)", check(h)) // Xcode 9.0: .none
  expectEqual(".some(.none)", check(h2))
}
}

protocol SwiftProtocol {}
CastsTests.test("Swift Protocol Metatypes don't self-conform") {
  let a = SwiftProtocol.self
  // `is P.Protocol` tests whether the argument is a subtype of P.
  // In particular, the protocol identifier `P.self` is such a subtype.
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectNotNil(runtimeCast(a, to: SwiftProtocol.Protocol.self)) // Fixed by rdar://58991956
  }
  expectNotNil(a as? SwiftProtocol.Protocol)
  expectTrue(a is SwiftProtocol.Protocol)
  blackhole(a as! SwiftProtocol.Protocol) // Should not trap

  // `is P.Type` tests conformance to P.  Protocols cannot conform to
  // protocols, so these always fail.
  expectNil(runtimeCast(a, to: SwiftProtocol.Type.self))
  expectNil(a as? SwiftProtocol.Type)
  expectFalse(a is SwiftProtocol.Type)
}

CastsTests.test("Self-conformance for Any.self") {
  let b = Any.self
  expectNotNil(runtimeCast(b, to: Any.Protocol.self))
  blackhole(b as! Any.Protocol) // Should not trap
  expectTrue(b is Any.Protocol)
  expectNotNil(b as? Any.Protocol)

  // Unlike most other protocols, Any.self does conform to Any
  expectNotNil(runtimeCast(b, to: Any.Type.self))
  expectNotNil(b as? Any.Type)
  expectTrue(b is Any.Type)
  blackhole(b as! Any.Type)
}

// rdar://59067748 (Error Protocol should self-conform in optimized casts)
CastsTests.test("Self-conformance for Error.self")
.skip(.custom({
      !_isDebugAssertConfiguration()
    },
    reason: "Cast optimizer breaks this test"))
.code {
  let c = Error.self
  expectNotNil(runtimeCast(c, to: Error.Protocol.self))
  expectNotNil(c as? Error.Protocol)
  expectTrue(c is Error.Protocol)
  blackhole(c as! Error.Protocol)

  // Unlike most other protocols, Error.self does conform to Error
  expectNotNil(runtimeCast(c, to: Error.Type.self))
  expectFailure { expectNotNil(c as? Error.Type) }
  expectFailure { expectTrue(c is Error.Type) }
  // blackhole(c as! Error.Type) // Should not trap, but currently does
}

// rdar://59067556 (Obj-C Protocol Metatypes should self-conform)
#if _runtime(_ObjC)
@objc protocol ObjCProtocol {}
CastsTests.test("ObjC Protocol Metatypes self-conform") {
  let a = ObjCProtocol.self
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectNotNil(runtimeCast(a, to: ObjCProtocol.Protocol.self))
  }
  expectNotNil(a as? ObjCProtocol.Protocol)
  expectTrue(a is ObjCProtocol.Protocol)
  blackhole(a as! ObjCProtocol.Protocol)

  // Unlike Swift protocols, ObjC protocols do conform to themselves
  expectFailure { expectNotNil(runtimeCast(a, to: ObjCProtocol.Type.self)) }
  expectFailure { expectNotNil(a as? ObjCProtocol.Type) }
  expectFailure { expectTrue(a is ObjCProtocol.Type) }
  // blackhole(a as! ObjCProtocol.Type) // Should not trap, but currently does
}
#endif

#if _runtime(_ObjC)
protocol NewStringProtocol {}
extension String: NewStringProtocol { }
CastsTests.test("String/NSString extension compat") {
  let x: Any = NSString()
  expectFailure { expectNotNil(runtimeCast(x, to: NewStringProtocol.self)) }
  expectFailure { expectNotNil(x as? NewStringProtocol) }
}
#endif

protocol P1999 {}
if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
CastsTests.test("Cast Any(Optional(class)) to Protocol type (SR-1999)") {
  class Foo: P1999 { }

  let optionalFoo : Foo? = Foo()
  let anyValue: Any = optionalFoo

  let foo1 = anyValue as? Foo
  expectNotNil(foo1)

  let foo2 = anyValue as? P1999
  expectNotNil(foo2)

  let foo3 = runtimeCast(anyValue, to: Foo.self)
  expectNotNil(foo3)

  let foo4 = runtimeCast(anyValue, to: P1999.self)
  expectNotNil(foo4)
}
}

#if _runtime(_ObjC)
CastsTests.test("Dict value casting (SR-2911)") {
  var dict: [AnyHashable: String] = [:]
  dict["Key"] = "Value"
  expectNotNil(dict["Key"] as? NSString)
  expectNotNil(runtimeCast(dict["Key"], to: NSString.self))
}
#endif

#if _runtime(_ObjC)
CastsTests.test("String coercions should work on Linux (SR-12020)") {
  let a = "abc" as Substring as NSString
  let b = "abc" as NSString
  expectEqual(a, b)

  let c = "abc" as Substring
  let d = c as? NSString
  let e = "abc" as? NSString
  expectEqual(d, e)

  let f = runtimeCast(d, to: NSString.self)
  expectEqual(e, f)
}
#endif

class ClassInt: Equatable, Hashable {
  private var tracker = LifetimeTracked(77)
  static func == (lhs: ClassInt, rhs: ClassInt) -> Bool {return true}
  func hash(into hasher: inout Hasher) {}
}
CastsTests.test("AnyHashable(Class) -> Obj-C -> Class")
.skip(.custom({
      !_isDebugAssertConfiguration()
    },
    reason: "Cast optimizer breaks this test"))
.code {
  let a = ClassInt()
  let b = runtimeCast(a, to: AnyHashable.self)!
  let c = _bridgeAnythingToObjectiveC(b)
  let d = /* SwiftValueBox(AnyHashable(ClassInt)) */ c as? ClassInt
  expectNotNil(d)
  let d2 = runtimeCast(c, to: ClassInt.self)
  expectNotNil(d2)
  let e = runtimeCast(/* SwiftValueBox(AnyHashable(ClassInt)) */ c, to: ClassInt.self)
  expectNotNil(e)
}

#if _runtime(_ObjC)
// rdar://58999120
CastsTests.test("Error -> NSError -> Protocol transitivity (SR-12095)") {
  enum NonConformingError: Error {
  case ok
  }

  let nonConformingError: Error = NonConformingError.ok

  // NSError conforms to CustomStringConvertible, so ...
  let conformingError = nonConformingError as? NSError
  expectTrue(conformingError is CustomStringConvertible)
  expectNotNil(conformingError as? CustomStringConvertible)

  // Our error type does not conform directly, but should conform
  // indirectly because of NSError...
  // Note: Known broken in both runtime and compiler.
  expectFailure { expectTrue(nonConformingError is CustomStringConvertible) }
  expectFailure { expectNotNil(nonConformingError as? CustomStringConvertible) }
}
#endif

#if _runtime(_ObjC)
CastsTests.test("Runtime crash casting Obj-C object to Obj-C protocol (rdar://16449805)") {
  // FIXME:  The reported crash was for `NSPoint(x:0, y:0) as? NSCoding`,
  // but NSPoint seems to not be available on 32-bit platforms.
  expectNotNil(NSString() as? NSCoding)
}
#endif

CastsTests.test("Casting Swift Error-conforming types to Error existentials") {
  enum Foo: Error {
  case OK
  case Broken
  }
  let a = Foo.Broken
  let b = a as? Error
  expectNotNil(b)
  let c = b as? Foo
  expectNotNil(c)
  let d = Foo.self as? Error.Type
  expectNotNil(d)
}

#if _runtime(_ObjC)
CastsTests.test("Casting NSError <-> Error") {
  @objc class Bar: NSError {
    init() {super.init(domain: "Bar", code: 99)}
    required init?(coder: NSCoder) {super.init(coder: coder)}
  }
  let e = Bar.self as? Error.Type
  expectNotNil(e)
  let f = Bar.self as? Bar.Type
  expectNotNil(f)
  let g = Bar() as? Error
  expectNotNil(g)
}
#endif

// Foundation's JSON handling makes heavy use of passing Any? inside of Any
// existentials.  That inspired the following three checks:
CastsTests.test("[Any(Any?)] -> [Any?] should prefer unwrapping source") {
  let a: Any? = nil
  let b: [Any] = [a as Any]
  let c = b as? [Any?]
  let d = c!
  let e = d[0]
  expectNil(e)
}

CastsTests.test("Any(Any?) -> Any? should prefer unwrapping source") {
  let a: Any? = nil
  let b: Any = a
  let c = b as? Any?
  let d = c!
  expectNil(d)
}

#if _runtime(_ObjC)
CastsTests.test("NSNull?.none -> Any? should set outer nil") {
  let a: NSNull? = nil
  let b = a as? Any?
  let c = b!
  expectNil(c)
}
#endif

CastsTests.test("Int??.some(nil) => Int??? should inject naturally") {
  let a: Int?? = .some(nil)
  let b = a as? Int???
  let c = b!
  let d = c!
  let e = d!
  expectNil(e)
}

CastsTests.test("Int??.some(nil) => String??? should inject naturally") {
  let a: Int?? = .some(nil)
  let b = runtimeCast(a, to: String???.self)
  let c = b!
  let d = c!
  let e = d!
  expectNil(e)
}

CastsTests.test("Int??.some(nil) => Any??? should inject naturally") {
  let a: Int?? = .some(nil)
  let b = a as? Any???
  let c = b!
  let d = c!
  let e = d!
  expectNil(e)
}

#if _runtime(_ObjC)
CastsTests.test("NSString -> String fast path") {
  let a = "short" as NSString
  expectNotNil(a as? String)
  let b = runtimeCast(a, to: String.self)
  expectNotNil(b)

  let c = "Long (adj) -- extended, large, the opposite of short" as NSString
  expectNotNil(c as? String)
  let d = runtimeCast(c, to: String.self)
  expectNotNil(d)

  let e = NSMutableString("not read-only")
  expectNotNil(e as? String)
  let f = runtimeCast(e, to: String.self)
  expectNotNil(f)

  let g = CFStringCreateWithCString(nil, "hello, world", CFStringBuiltInEncodings.UTF8.rawValue)
  expectNotNil(g as? String)
  let h = runtimeCast(g, to: String.self)
  expectNotNil(h)

  let i = CFStringCreateMutable(nil, 0)
  expectNotNil(i as? String)
  let j = runtimeCast(i, to: String.self)
  expectNotNil(j)
}
#endif

// This fails in optimized builds because after inlining `runtimeCast`,
// the resulting SIL cast operation is left in a form that IRGen can't
// correctly handle.
//CastsTests.test("Optimized metatype -> AnyObject cast") {
//  struct StructInt { }
//  let a = StructInt.self
//  let b = runtimeCast(a, to: AnyObject.self)
//  expectNotNil(b)
//}

CastsTests.test("Any.Protocol") {
  class C {}
  struct S {}
  func isAnyProtocol<T>(_ type: T.Type) -> Bool {
    let result = T.self is Any.Protocol
		if result {
			// `as!` should succeed if `is` does
			blackhole(T.self as! Any.Protocol)
		}
    return result
  }
  func isAnyType<T>(_ type: T.Type) -> Bool {
    return T.self is Any.Type
  }
  func isType<T,U>(_ type: T.Type, to: U.Type) -> Bool {
    return T.self is U.Type
  }

  expectTrue(Int.self is Any.Type)
  expectNotNil(Int.self as? Any.Type)
  expectTrue(isAnyType(Int.self))
  expectFalse(Int.self is Any.Protocol)
  expectNil(Int.self as? Any.Protocol)
  expectFalse(isAnyProtocol(Int.self))
  expectFalse(isType(Int.self, to: Any.self))

  expectTrue(C.self is Any.Type)
  expectNotNil(C.self as? Any.Type)
  expectTrue(isAnyType(C.self))
  expectFalse(C.self is Any.Protocol)
  expectNil(C.self as? Any.Protocol)
  expectFalse(isAnyProtocol(C.self))
  expectFalse(isType(C.self, to: Any.self))

  expectTrue(S.self is Any.Type)
  expectNotNil(S.self as? Any.Type)
  expectTrue(isAnyType(S.self))
  expectFalse(S.self is Any.Protocol)
  expectNil(S.self as? Any.Protocol)
  expectFalse(isAnyProtocol(S.self))
  expectFalse(isType(S.self, to: Any.self))

  expectTrue(Any.self is Any.Type)
  expectNotNil(Any.self as? Any.Type)
  expectTrue(isAnyType(Any.self))
  expectTrue(Any.self is Any.Protocol)
  expectNotNil(Any.self as? Any.Protocol)
  expectTrue(isAnyProtocol(Any.self))
  expectTrue(isType(Any.self, to: Any.self))

  expectTrue(Any?.self is Any.Type)
  expectNotNil(Any?.self as? Any.Type)
  expectTrue(isAnyType(Any?.self))
  expectFalse(Any?.self is Any.Protocol)
  expectNil(Any?.self as? Any.Protocol)
  expectFalse(isAnyProtocol(Any?.self))
  expectFalse(isType(Any?.self, to: Any.self))
}

CastsTests.test("Async function types") {
  let asyncFnType: Any.Type = (() async -> Void).self
  let fnType: Any.Type = (() -> Void).self

  expectTrue(fnType is (() -> Void).Type)
  expectTrue(asyncFnType is (() async -> Void).Type)
  expectFalse(fnType is (() async -> Void).Type)
  expectFalse(asyncFnType is (() -> Void).Type)
}

runAllTests()

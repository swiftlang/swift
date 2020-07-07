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
/// Contains tests for conversions between types which shouldn't trap.
///
// -----------------------------------------------------------------------------
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

private func blackhole<T>(_ t: T) {}

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

CastsTests.test("Dynamic casts of CF types to protocol existentials")
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "This test behaves unpredictably in optimized mode."))
  .code {
  expectTrue(isP(10 as Int))

  // FIXME: SR-2289: dynamic casting of CF types to protocol existentials
  // should work, but there is a bug in the runtime that prevents them from
  // working.
  expectFailure {
    expectTrue(isP(CFBitVector.makeImmutable(from: [10, 20])))
  }
  expectFailure {
    expectTrue(isP(CFMutableBitVector.makeMutable(from: [10, 20])))
  }
}
#endif

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

runAllTests()

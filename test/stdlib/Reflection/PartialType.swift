// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection_runtime
// UNSUPPORTED: freestanding
// XFAIL: OS=wasi

import StdlibUnittest
import _Runtime
import Reflection

let suite = TestSuite("PartialType")

//===----------------------------------------------------------------------===//
// Basic
//===----------------------------------------------------------------------===//

struct TypeWhoDoesNotConformToHashable {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("basic creation") {
    let intPTy = Type(Int.self).partial!
    let int = intPTy.create()
    expectNotNil(int)
    expectEqual(int!.swiftType, Int.self)

    let arrPTy = Type([Void].self).partial!
    let intArr = arrPTy.create(with: Int.self)
    expectNotNil(intArr)
    expectEqual(intArr!.swiftType, [Int].self)

    let dictPTy = Type([AnyHashable: Any].self).partial!
    let goodDict = dictPTy.create(with: String.self, Int.self)
    expectNotNil(goodDict)
    expectEqual(goodDict!.swiftType, [String: Int].self)

    let badDict = dictPTy.create(with: TypeWhoDoesNotConformToHashable.self, Int.self)
    expectNil(badDict)
  }
}

//===----------------------------------------------------------------------===//
// Protocol conformance constraints
//===----------------------------------------------------------------------===//

protocol MyProto {}

extension Int: MyProto {}
extension Bool: MyProto {}

struct ProtoGeneric<T: MyProto> {}

struct ProtoGeneric2<T: Collection> where T.Element: Hashable {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("protocol conformance constraint creation") {
    let protoGenericPTy = Type(ProtoGeneric<Bool>.self).partial!

    let a = protoGenericPTy.create(with: Int.self)
    expectNotNil(a)
    expectEqual(a!.swiftType, ProtoGeneric<Int>.self)

    let b = protoGenericPTy.create(with: String.self)
    expectNil(b)

    let protoGeneric2PTy = Type(ProtoGeneric2<[String]>.self).partial!

    let c = protoGeneric2PTy.create(with: [Int].self)
    expectNotNil(c)
    expectEqual(c!.swiftType, ProtoGeneric2<[Int]>.self)

    let d = protoGeneric2PTy.create(with: [TypeWhoDoesNotConformToHashable].self)
    expectNil(d)
  }
}

//===----------------------------------------------------------------------===//
// Same type constraints
//===----------------------------------------------------------------------===//

struct SameTypeGeneric1<T> {}

extension SameTypeGeneric1 where T == String {
  struct Nested {}
}

struct SameTypeGeneric2<T: Collection> where T.Element == Int {}

struct SameTypeGeneric3<T, U> {}

extension SameTypeGeneric3 where T == U? {
  struct Nested {}
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("same type constraint creation") {
    // 1. Concrete same types

    let nestedPTy = Type(SameTypeGeneric1<String>.Nested.self).partial!

    let a = nestedPTy.create()
    expectNotNil(a)
    expectEqual(a!.swiftType, SameTypeGeneric1<String>.Nested.self)

    let b = nestedPTy.create(with: Int.self)
    expectNil(b)

    let c = nestedPTy.create(with: String.self)
    expectNil(c)

    // 2. Associated type same types

    let stg2PTy = Type(SameTypeGeneric2<Set<Int>>.self).partial!

    let d = stg2PTy.create(with: [Int].self)
    expectNotNil(d)
    expectEqual(d!.swiftType, SameTypeGeneric2<[Int]>.self)

    let e = stg2PTy.create(with: Bool.self)
    expectNil(e)

    let f = stg2PTy.create(with: [String].self)
    expectNil(f)

    // 3. Generic same types

    let nested2PTy = Type(SameTypeGeneric3<String?, String>.Nested.self).partial!

    let g = nested2PTy.create(with: Int.self)
    expectNotNil(g)
    expectEqual(g!.swiftType, SameTypeGeneric3<Int?, Int>.Nested.self)

    let h = nested2PTy.create()
    expectNil(h)

    let i = nested2PTy.create(with: Int?.self)
    expectNotNil(i)
    expectEqual(i!.swiftType, SameTypeGeneric3<Int??, Int?>.Nested.self)
  }
}

//===----------------------------------------------------------------------===//
// Layout constraints
//===----------------------------------------------------------------------===//

class LayoutClass1 {}
class LayoutClass2 {}

struct LayoutGeneric<T: AnyObject> {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("layout constraint creation") {
    let layoutGenericPTy = Type(LayoutGeneric<LayoutClass1>.self).partial!

    let a = layoutGenericPTy.create(with: Int.self)
    expectNil(a)

    let b = layoutGenericPTy.create(with: [Int].self)
    expectNil(b)

    let c = layoutGenericPTy.create()
    expectNil(c)

    let d = layoutGenericPTy.create(with: LayoutClass2.self)
    expectNotNil(d)
    expectEqual(d!.swiftType, LayoutGeneric<LayoutClass2>.self)
  }
}

//===----------------------------------------------------------------------===//
// Base class constraints
//===----------------------------------------------------------------------===//

class UnrelatedClass {}

class BaseClass1 {}
class SubClass1: BaseClass1 {}
struct BaseClassGeneric1<T: BaseClass1> {}

class BaseClass2<T> {}
class SubClass2: BaseClass2<Int> {}
struct BaseClassGeneric2<T: BaseClass2<Int>> {}
struct Weird<T> {}

extension Weird where T: Equatable {
  class SubSubClass2: BaseClass2<T> {}
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("base class constraint creation") {
    let one = Type(BaseClassGeneric1<BaseClass1>.self).partial!

    let a = one.create(with: Int.self)
    expectNil(a)

    let b = one.create(with: SubClass1.self)
    expectNotNil(b)
    expectEqual(b!.swiftType, BaseClassGeneric1<SubClass1>.self)

    let c = one.create(with: BaseClass1.self)
    expectNotNil(c)
    expectEqual(c!.swiftType, BaseClassGeneric1<BaseClass1>.self)

    let two = Type(BaseClassGeneric2<BaseClass2<Int>>.self).partial!

    let d = two.create(with: Int.self)
    expectNil(d)

    let e = two.create(with: BaseClass2<String>.self)
    expectNil(e)

    let f = two.create(with: BaseClass2<Int>.self)
    expectNotNil(f)
    expectEqual(f!.swiftType, BaseClassGeneric2<BaseClass2<Int>>.self)

    let g = two.create(with: SubClass2.self)
    expectNotNil(g)
    expectEqual(g!.swiftType, BaseClassGeneric2<SubClass2>.self)

    let h = two.create(with: Weird<String>.SubSubClass2.self)
    expectNil(h)

    let i = two.create(with: Weird<Int>.SubSubClass2.self)
    expectNotNil(i)
    expectEqual(i!.swiftType, BaseClassGeneric2<Weird<Int>.SubSubClass2>.self)
  }
}

runAllTests()

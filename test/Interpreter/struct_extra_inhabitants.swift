// RUN: %empty-directory(%t)

// -- build resilient library
// RUN: %target-build-swift -force-single-frontend-invocation -Xfrontend -enable-resilience -module-name ExtraInhabitantResilientTypes -emit-module-path %t/ExtraInhabitantResilientTypes.swiftmodule -parse-as-library -c -o %t/ExtraInhabitantResilientTypes.o %S/Inputs/struct_extra_inhabitants_ExtraInhabitantResilientTypes.swift

// -- run tests
// RUN: %target-build-swift -parse-stdlib -Xfrontend -verify-type-layout -Xfrontend PairWithPointerFirst -Xfrontend -verify-type-layout -Xfrontend PairWithPointerSecond -Xfrontend -verify-type-layout -Xfrontend PairWithPointerSecondAndPhantomParam_Int -Xfrontend -verify-type-layout -Xfrontend GenericPairWithPointerFirst_Int -Xfrontend -verify-type-layout -Xfrontend GenericPairWithPointerFirst_AnyObject -Xfrontend -verify-type-layout -Xfrontend GenericPairWithPointerSecond_Int -Xfrontend -verify-type-layout -Xfrontend GenericPairWithPointerSecond_AnyObject -Xfrontend -verify-type-layout -Xfrontend StringAlike32 -Xfrontend -verify-type-layout -Xfrontend StringAlike64 -I %t -o %t/a.out.tests %s %t/ExtraInhabitantResilientTypes.o
// RUN: %target-run %t/a.out.tests 2>&1

// Type layout verifier is only compiled into the runtime in asserts builds.
// REQUIRES: swift_stdlib_asserts

// CHECK-NOT: Type verification

import Swift
import ExtraInhabitantResilientTypes
import StdlibUnittest

// Enum layout should use extra inhabitants from any struct field
// for fixed-layout types.
struct PairWithPointerFirst {
  var a: AnyObject
  var b: Int
}

struct PairWithPointerSecond {
  var a: Int
  var b: AnyObject
}

struct PairWithPointerSecondAndPhantomParam<X> {
  var a: Int
  var b: AnyObject
}

struct StringAlike64 {
  var a: Int
  var b: Builtin.BridgeObject
}
struct StringAlike32 {
  var a,b,c: Int
  var d: Builtin.BridgeObject
}

// TODO: Runtime struct instantiation still only considers the first argument

struct GenericPair<T, U> {
  var a: T
  var b: U
}

struct GenericSamePair<T> {
  var a: T
  var b: T
}

struct GenericPairPlusPointer<T, U> {
  var a: T
  var b: U
  var c: UnsafeRawPointer
}

struct GenericSamePairPlusPointer<T> {
  var a: T
  var b: T
  var c: UnsafeRawPointer
}

struct GenericPairWithPointerFirst<T> {
  var a: AnyObject
  var b: T
}

struct GenericPairWithPointerSecond<T> {
  var a: T
  var b: AnyObject
}

struct GenericFullHouse<T, U> {
  var a, b, c: T
  var d, e: U
}

struct ResilientPairWithXIFirst {
  var a: ResilientXI
  var b: ResilientNoXI
}

struct ResilientPairWithXISecond {
  var a: ResilientNoXI
  var b: ResilientXI
}

// Typealiases for the type layout verifier

typealias PairWithPointerSecondAndPhantomParam_Int
 = PairWithPointerSecondAndPhantomParam<Int>

typealias GenericPairWithPointerFirst_Int
 = GenericPairWithPointerFirst<Int>

typealias GenericPairWithPointerFirst_AnyObject
 = GenericPairWithPointerFirst<AnyObject>

typealias GenericPairWithPointerSecond_Int
 = GenericPairWithPointerSecond<Int>

typealias GenericPairWithPointerSecond_AnyObject
 = GenericPairWithPointerSecond<AnyObject>

var tests = TestSuite("extra inhabitants of structs")

@inline(never)
func expectHasExtraInhabitant<T>(_: T.Type, nil: T?,
                                 file: String = #file, line: UInt = #line) {
  expectEqual(MemoryLayout<T>.size, MemoryLayout<T?>.size,
              "\(T.self) has extra inhabitant",
              file: file, line: line)

  expectNil(Optional<T>.none,
            "\(T.self) extra inhabitant should agree in generic and concrete " +
            "context")
}

func expectHasNoExtraInhabitant<T>(_: T.Type,
                                   file: String = #file, line: UInt = #line) {
  expectNotEqual(MemoryLayout<T>.size, MemoryLayout<T?>.size,
                 "\(T.self) does not have extra inhabitant",
                 file: file, line: line)
}

tests.test("types that have extra inhabitants") {
  expectHasExtraInhabitant(PairWithPointerFirst.self, nil: nil)
  expectHasExtraInhabitant(PairWithPointerSecond.self, nil: nil)
  expectHasExtraInhabitant(PairWithPointerSecondAndPhantomParam<Int>.self, nil: nil)
  expectHasExtraInhabitant(PairWithPointerSecondAndPhantomParam<AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairWithPointerFirst<Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairWithPointerFirst<AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairWithPointerSecond<Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairWithPointerSecond<AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(ResilientPairWithXIFirst.self, nil: nil)
  expectHasExtraInhabitant(ResilientPairWithXISecond.self, nil: nil)
  expectHasExtraInhabitant(StringAlike64.self, nil: nil)
  expectHasExtraInhabitant(StringAlike32.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<AnyObject, Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<Int, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<AnyObject, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<AnyObject, UnsafeRawPointer>.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<UnsafeRawPointer, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPair<UnsafeRawPointer, UnsafeRawPointer>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairPlusPointer<Int, Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairPlusPointer<AnyObject, Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairPlusPointer<Int, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericPairPlusPointer<AnyObject, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericSamePair<UnsafeRawPointer>.self, nil: nil)
  expectHasExtraInhabitant(GenericSamePair<AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericSamePairPlusPointer<UnsafeRawPointer>.self, nil: nil)
  expectHasExtraInhabitant(GenericSamePairPlusPointer<AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericSamePairPlusPointer<Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<AnyObject, Int>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<Int, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<AnyObject, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<AnyObject, UnsafeRawPointer>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<UnsafeRawPointer, AnyObject>.self, nil: nil)
  expectHasExtraInhabitant(GenericFullHouse<UnsafeRawPointer, UnsafeRawPointer>.self, nil: nil)
}

runAllTests()


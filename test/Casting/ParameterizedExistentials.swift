// ParameterizedExistentials.swift - Casting tests for generalized existentials
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
/// Tests for parameterized existential type conversions.
///
// -----------------------------------------------------------------------------
// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -swift-version 5 -g -Onone -Xfrontend -disable-availability-checking -module-name a -c %s -o %t/ParameterizedExistentials.swift.Onone.o
// RUN: %target-swiftc_driver %t/ParameterizedExistentials.swift.Onone.o -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -swift-version 5 -g -O -Xfrontend -disable-availability-checking -module-name a -c %s -o %t/ParameterizedExistentials.swift.O.o
// RUN: %target-swiftc_driver %t/ParameterizedExistentials.swift.O.o -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
//
// REQUIRES: executable_test
// This test requires the new existential shape metadata accessors which are
// not available in on-device runtimes, or in the back-deployment runtime.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Swift

import StdlibUnittest

protocol Holder<T> {
  associatedtype T
  var value: T { get }
}

struct GenericHolder<T>: Holder {
  var value: T

  init(value: T) { self.value = value}
}

protocol PairType<
  T,
  U,
> {
  associatedtype T
  associatedtype U

  var first: T { get }
  var second: U { get }
}

struct Pair<T, U>: PairType {
  var value: (T, U)

  var first: T { self.value.0 }
  var second: U { self.value.1 }

  init(value: (T, U)) { self.value = value}
}

final class ReferencePair<T, U>: PairType {
  var first: T
  var second: U

  init(value: (T, U)) { (self.first, self.second) = value }
}

let tests = TestSuite("ParameterizedExistentials")

tests.test("Parameterized existential casting basics work") {
  let a = GenericHolder(value: 5) as any Holder<Int>
  let b = GenericHolder(value: 5) as! any Holder<Int>
  expectEqual(a.value, b.value)
  let c = GenericHolder(value: 5) as? any Holder<Int>
  expectNotNil(c)
  let d = GenericHolder(value: 5) as? any Holder<String>
  expectNil(d)
}

tests.test("Metatype existential casting basics work") {
  let a = GenericHolder<Int>.self as any Holder<Int>.Type
  let b = GenericHolder<Int>.self as! any Holder<Int>.Type
  expectTrue(a == b)
  let c = GenericHolder<Int>.self as? any Holder<Int>.Type
  expectNotNil(c)
  let d = GenericHolder<Int>.self as? any Holder<String>.Type
  expectNil(d)
}

tests.test("Existential box should maintain identity") {
  let a = Pair(value: ("Hello", 42))
  var b/*ox*/ = a as? any PairType<String, Int>
  expectNotNil(b)
  expectEqual(a.value, (b!.first, b!.second))

  let c = ReferencePair(value: ("Goodbye", 24))
  let d = c as? any PairType<String, Int>
  expectNotNil(d)

  b = d!
  expectEqual(b!.first, d!.first)
  expectEqual(b!.second, d!.second)

  let e = b as? ReferencePair<String, Int>
  expectNotNil(e)
  expectTrue(e! === c)
}

runAllTests()

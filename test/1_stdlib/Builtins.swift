//===--- Builtins.swift - Tests for our Builtin wrappers ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/Builtins
// RUN: %target-run %t/Builtins
// XFAIL: interpret

import Swift
import SwiftShims
import StdlibUnittest

var tests = TestSuite("Builtins")

class X {}

func cast(a: Builtin.NativeObject) -> UnsafePointer<HeapObject> {
  let b: X = Builtin.castFromNativeObject(a)
  return UnsafePointer(Builtin.bridgeToRawPointer(b))
}

func cast(a: Builtin.NativeObject?) -> UnsafePointer<HeapObject> {
  return UnsafePointer(Builtin.reinterpretCast(a) as Builtin.RawPointer)
}

tests.test("_isUniquelyReferenced/NativeObject") {
  var a: Builtin.NativeObject = Builtin.castToNativeObject(X())
  expectNotEqual(0, _swift_isUniquelyReferenced_nonNull_native(cast(a)))
  var b = a
  expectFalse(_swift_isUniquelyReferenced_nonNull_native(cast(a)) != 0)
  expectFalse(_swift_isUniquelyReferenced_nonNull_native(cast(b)) != 0)
}

tests.test("_isUniquelyReferenced/OptionalNativeObject") {
  var a: Builtin.NativeObject? = Builtin.castToNativeObject(X())
  StdlibUnittest.expectTrue(_swift_isUniquelyReferenced_native(cast(a)) != 0)
  var b = a
  expectFalse(_swift_isUniquelyReferenced_native(cast(a)) != 0)
  expectFalse(_swift_isUniquelyReferenced_native(cast(b)) != 0)
  var x: Builtin.NativeObject? = nil
  expectFalse(_swift_isUniquelyReferenced_native(cast(x)) != 0)
}

runAllTests()

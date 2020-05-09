//===--- subclass_existentials_objc.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: objc_interop
//

import StdlibUnittest
import Foundation

// FIXME: Actually write proper tests here.

func cast<T, U>(_ t: T, to: U.Type) -> U? {
  return t as? U
}

protocol CP : class {}
@objc protocol OP {}

class C {}

class D : C, OP {}

var SubclassExistentialsTestSuite = TestSuite("SubclassExistentials")

SubclassExistentialsTestSuite.test("Metatype self-conformance") {
  expectFalse(CP.self is AnyObject.Type)
  expectEqual(OP.self, OP.self as AnyObject.Type)

  expectNil(cast(CP.self, to: AnyObject.Type.self))
  // FIXME
  expectNil(cast(OP.self, to: AnyObject.Type.self))

  // FIXME: Sema says 'always true', runtime says false.
  //
  // Runtime code had a FIXME in it already, so I think Sema is right.
  expectFalse(OP.self is AnyObject.Type)
  expectFalse((C & OP).self is AnyObject.Type)
  expectFalse((C & OP).self is OP.Type)

  expectTrue(D.self is (C & OP).Type)
  expectFalse(OP.self is (C & OP).Type)
}

runAllTests()

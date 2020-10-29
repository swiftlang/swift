// ObjCClassConstants.swift - Tests for class constant casts w/ Obj-C
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
// RUN: %target-clang -fmodules -c %S/Inputs/ObjCClassConstants/ObjCClassConstants.m -o %t/ObjCClassConstants.objc.o
//
// RUN: %target-build-swift -swift-version 5 -g -Onone -module-name a -I %S/Inputs/ObjCClassConstants -c %s -o %t/ObjCClassConstants.swift.Onone.o
// RUN: %target-swiftc_driver %t/ObjCClassConstants.objc.o %t/ObjCClassConstants.swift.Onone.o -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -swift-version 5 -g -O -module-name a -I %S/Inputs/ObjCClassConstants -c %s -o %t/ObjCClassConstants.swift.O.o
// RUN: %target-swiftc_driver %t/ObjCClassConstants.objc.o %t/ObjCClassConstants.swift.O.o -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
//
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib

import Foundation
import ObjCClassConstants
import Swift

import StdlibUnittest

let tests = TestSuite("ObjCClassConstants")

tests.test("ObjC and Swift type lookups should agree") {
  // Look up class object from Obj-C (in an array)
  // Extract first element, then cast to AnyObject
  let a = OCClassConstants.classes
  let b = a[0]
  let c = b as? AnyObject
  expectNotNil(c)
  let d = c!

  // Look up class object from Swift, cast to AnyObject
  let e = OCClassConstants.self
  let f = e as? AnyObject
  expectNotNil(f)
  let g = f!

  // Should be exact same pointer
  expectTrue(d === g)
}

tests.test("ObjC and Swift type lookups should agree (with array cast to AnyObject)") {
  // Look up class object from Obj-C (in an array)
  // Cast array to AnyObject, then extract first element
  let a = OCClassConstants.classes
  let b = a as? [AnyObject]
  expectNotNil(b)
  let c = b!
  let d = c[0]

  // Look up class object from Swift, cast to AnyObject
  let e = OCClassConstants.self
  let f = e as? AnyObject
  expectNotNil(f)
  let g = f!

  // Should be exact same pointer
  expectTrue(d === g)
}

runAllTests()

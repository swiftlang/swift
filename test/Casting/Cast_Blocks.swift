// Casts.swift - Tests for conversion between types.
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
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
// RUN: %clang %target-cc-options -isysroot %sdk -fobjc-arc %S/Inputs/Cast_Blocks/Cast_Blocks.m -c -o %t/Cast_Blocks.o -g
//
// RUN: %target-build-swift -I %S/Inputs/Cast_Blocks %t/Cast_Blocks.o -swift-version 6 -g -Onone  -module-name a %s -o %t/a.swift6.Onone.out
// RUN: %target-codesign %t/a.swift6.Onone.out
// RUN: %target-run %t/a.swift6.Onone.out
//
// RUN: %target-build-swift -I %S/Inputs/Cast_Blocks %t/Cast_Blocks.o -swift-version 6 -g -O  -module-name a %s -o %t/a.swift6.O.out
// RUN: %target-codesign %t/a.swift6.O.out
// RUN: %target-run %t/a.swift6.O.out
//
// RUN: %target-build-swift -I %S/Inputs/Cast_Blocks %t/Cast_Blocks.o -swift-version 5 -g -Onone  -module-name a %s -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -I %S/Inputs/Cast_Blocks %t/Cast_Blocks.o -swift-version 5 -g -O  -module-name a %s -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
//
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Foundation
import Cast_Blocks

fileprivate func SwiftThinksObjectIsSwiftValue<T>(_ t: T) -> Bool {
  let type = "\(type(of: t))"
  return type == "__SwiftValue"
}

let CastsTests = TestSuite("Cast_Blocks")

CastsTests.test("block closures should bridge without __SwiftValue")
{
  let x: @convention(block) () -> Void = {}

  expectFalse(ObjCThinksObjectIsSwiftValue(x))

  expectFalse(SwiftThinksObjectIsSwiftValue(x))

  ObjCCanCallBlock(x);
}

// Bug:  @convention(block) closure used to be incorrectly wrapped in
// SwiftValue box when bridged to Objective-C as a member of an array
CastsTests.test("block closures in array should bridge without __SwiftValue")
{
  let f: @convention(block) () -> Void = {}
  let x = ([f] as NSArray)[0]

  expectFalse(ObjCThinksObjectIsSwiftValue(x))
  expectFalse(SwiftThinksObjectIsSwiftValue(x))
  ObjCCanCallBlock(x);
}


runAllTests()

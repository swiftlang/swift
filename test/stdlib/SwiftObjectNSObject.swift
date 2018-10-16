//===--- SwiftObjectNSObject.swift - Test SwiftObject's NSObject interop --===//
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

// RUN: %empty-directory(%t)
// 
// RUN: %target-clang %S/Inputs/SwiftObjectNSObject/SwiftObjectNSObject.m -c -o %t/SwiftObjectNSObject.o -g
// RUN: %target-build-swift %s -I %S/Inputs/SwiftObjectNSObject/ -Xlinker %t/SwiftObjectNSObject.o -o %t/SwiftObjectNSObject
// RUN: %target-codesign %t/SwiftObjectNSObject
// RUN: %target-run %t/SwiftObjectNSObject 2> %t/log.txt
// RUN: %FileCheck %s < %t/log.txt
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

class C { 
  @objc func cInstanceMethod() -> Int { return 1 }
  @objc class func cClassMethod() -> Int { return 2 }
  @objc func cInstanceOverride() -> Int { return 3 }
  @objc class func cClassOverride() -> Int { return 4 }
}
class D : C {
  @objc func dInstanceMethod() -> Int { return 5 }
  @objc class func dClassMethod() -> Int { return 6 }
  @objc override func cInstanceOverride() -> Int { return 7 }
  @objc override class func cClassOverride() -> Int { return 8 }
}

@_silgen_name("TestSwiftObjectNSObject") 
func TestSwiftObjectNSObject(_ c: C, _ d: D)

// This check is for NSLog() output from TestSwiftObjectNSObject().
// CHECK: c ##SwiftObjectNSObject.C##
// CHECK-NEXT: d ##SwiftObjectNSObject.D##
// CHECK-NEXT: S ##{{(Swift._)?}}SwiftObject##

TestSwiftObjectNSObject(C(), D())
// does not return

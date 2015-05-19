//===--- SwiftObjectNSObject.swift - Test SwiftObject's NSObject interop --===//
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

// RUN: rm -rf %t  &&  mkdir -p %t
// 
// RUN: %target-clang %S/Inputs/SwiftObjectNSObject/SwiftObjectNSObject.m -c -o %t/SwiftObjectNSObject.o -g
// RUN: %target-build-swift %s -I %S/Inputs/SwiftObjectNSObject/ -Xlinker %t/SwiftObjectNSObject.o -o %t/SwiftObjectNSObject
// RUN: %target-run %t/SwiftObjectNSObject

// REQUIRES: objc_interop

import Foundation

class C { 
  @objc func cInstanceMethod() { }
  @objc class func cClassMethod() { }
}
class D : C {
  @objc func dInstanceMethod() { }
  @objc class func dClassMethod() { }

}

@asmname("TestSwiftObjectNSObject") 
func TestSwiftObjectNSObject(c: C, _ d: D)

TestSwiftObjectNSObject(C(), D())
// does not return

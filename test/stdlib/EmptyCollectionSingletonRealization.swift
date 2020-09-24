// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// Test to make sure that empty collections don't cause a crash if we smuggle
// them into the ObjC runtime without doing anything that would trigger
// realization. The ObjC runtime expects all classes to have been realized
// (i.e. runtime data structures initialized, triggered the first time a class
// is accessed or used) before being queried in any way.
//
// Note: this test deliberately avoids StdlibUnittest to make sure
// no other code runs that might inadvertently trigger realization behind our
// back.

@objc protocol P {}


if #available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
  do {
    let d: [NSObject: NSObject] = [:]
    let c: AnyClass? = object_getClass(d)
    let conforms = class_conformsToProtocol(c, P.self)
    print("Dictionary: ", conforms) // CHECK: Dictionary: false
  }

  do {
    let a: [NSObject] = []
    let c: AnyClass? = object_getClass(a)
    let p = objc_getProtocol("NSObject")
    let conforms = class_conformsToProtocol(c, p)
    print("Array:", conforms) // CHECK: Array: false
  }

  do {
    let s: Set<NSObject> = []
    let c: AnyClass? = object_getClass(s)
    let p = objc_getProtocol("NSObject")
    let conforms = class_conformsToProtocol(c, p)
    print("Set:", conforms) // CHECK: Set: false
  }
} else {
  // When testing against an older runtime that doesn't have this fix, lie.
  print("Dictionary: false")
  print("Array: false")
  print("Set: false")
}

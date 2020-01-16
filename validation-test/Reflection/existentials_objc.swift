// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/existentials_objc
// RUN: %target-codesign %t/existentials_objc
// RUN: %target-run %target-swift-reflection-test %t/existentials_objc > %t.txt
// RUN: grep SkipTheTest %t.txt || %FileCheck %s < %t.txt

// REQUIRES: objc_interop
// REQUIRES: executable_test

import Foundation

/*
   This file pokes at the swift_reflection_projectExistential API
   of the SwiftRemoteMirror library.
*/

import SwiftReflectionTest

if #available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *) {
  // Imported class wrapped in AnyObject

  // CHECK: Type reference:
  // CHECK: (objective_c_class name=NSObject)
  reflect(object: NSObject())

  // Tagged pointer wrapped in AnyObject
  // CHECK: Type reference:
  // CHECK: (objective_c_class name=__NSCFNumber)
  reflect(object: NSNumber(123))
} else {
  // The Swift 5.0 libraries don't support this test.
  class SkipTheTest {}
  reflect(object: SkipTheTest())
}

doneReflecting()

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/existentials_objc
// RUN: %target-codesign %t/existentials_objc
// RUN: %target-run %target-swift-reflection-test %t/existentials_objc | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

import Foundation

/*
   This file pokes at the swift_reflection_projectExistential API
   of the SwiftRemoteMirror library.
*/

import SwiftReflectionTest

// Imported class wrapped in AnyObject

// CHECK: Type reference:
// CHECK: (objective_c_class name=NSObject)
reflect(object: NSObject())

// Tagged pointer wrapped in AnyObject
// CHECK: Type reference:
// CHECK: (objective_c_class name=__NSCFNumber)
reflect(object: NSNumber(123))

doneReflecting()

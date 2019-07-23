// RUN: %empty-directory(%t)
//
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift %t/main.swift -I %S/Inputs/custom-modules/ -o %t/OS_objects -Xfrontend -disable-access-control
// RUN: %target-codesign %t/OS_objects
// RUN: %target-run %t/OS_objects 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

// Note: Test the use of the Clang objc_runtime_visible attribute via
// known OS objects on Darwin.

import ObjectiveC
import DispatchObjects

// CHECK: Get current queue
print("Get current queue")
// TODO: Properly implement generalized dynamic casts from Any to
// runtime-visible classes. `as AnyObject` should be unnecessary here.
let obj = dispatch_get_current_queue() as AnyObject

// CHECK-NEXT: Object is a dispatch queue
if let q = obj as? OS_dispatch_queue {
  print("Object is a dispatch queue")
}

// CHECK-NEXT: Object is an OS object
if let q = obj as? OS_object {
  print("Object is an OS object")
}

// CHECK-NEXT: Object is an NSObject
if let q = obj as? NSObject {
  print("Object is an NSObject")
}

// CHECK-NEXT: Object is not a dispatch source
if let q = obj as? OS_dispatch_source {
  print("Object is a dispatch source!?!?")
} else {
  print("Object is not a dispatch source")
}

// CHECK-NEXT: DONE
print("DONE");

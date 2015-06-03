// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

print("Begin test.");
// CHECK:      Begin test.

let str = "Created as String"
let nsstr: NSString = "Created as NSString"

// CHECK-NEXT: Created as NSString
let cfstr: CFString = nsstr
print(cfstr)

// CHECK-NEXT: ----
print("----")

// CHECK-NEXT: Created as NSString
let cfstrAsNSStr: NSString = cfstr
print(cfstrAsNSStr)

// CHECK-NEXT: Created as NSString
let cfstrAsStr = cfstr as NSString as String
print(cfstrAsStr)

// CHECK-NEXT: Created as String
let cfstr2: CFString = str as NSString
print(cfstr2)

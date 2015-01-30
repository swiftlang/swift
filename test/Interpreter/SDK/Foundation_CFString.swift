// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

println("Begin test.");
// CHECK:      Begin test.

let str = "Created as String"
let nsstr: NSString = "Created as NSString"

// CHECK-NEXT: Created as NSString
let cfstr: CFString = nsstr
println(cfstr)

// CHECK-NEXT: ----
println("----")

// CHECK-NEXT: Created as NSString
let cfstrAsNSStr: NSString = cfstr
println(cfstrAsNSStr)

// CHECK-NEXT: Created as NSString
let cfstrAsStr = cfstr as NSString as String
println(cfstrAsStr)

// CHECK-NEXT: Created as String
let cfstr2: CFString = str as NSString
println(cfstr2)

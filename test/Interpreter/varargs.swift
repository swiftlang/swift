// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

import Foundation

func vf(_ params: CVarArg...) {
	print("OK")
}

var a: [AnyObject]! = ["a" as NSString]
var s: String! = "s"

vf(a as NSArray)
// CHECK: OK
vf(s as NSString)
// CHECK: OK

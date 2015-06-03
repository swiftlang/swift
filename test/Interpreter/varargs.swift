// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

import Foundation

func vf(params: CVarArgType...) {
	print("OK")
}

var a: [AnyObject]! = ["a"]
var s: String! = "s"

vf(a as NSArray)
// CHECK: OK
vf(s as NSString)
// CHECK: OK

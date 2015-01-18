// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func vf(params: CVarArgType...) {
	println("OK")
}

var a: [AnyObject]! = ["a"]
var s: String! = "s"

vf(a as NSArray)
// CHECK: OK
vf(s as NSString)
// CHECK: OK

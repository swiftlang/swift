// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

var str : NSString? = nil
var url : NSURL? = nil

println("\(str == nil) \(nil == url) \(str == url)")
// CHECK: true true true

str = "abc"
url = NSURL(string: "file:///")
println("\(str == nil) \(nil == url)")
// CHECK: false false

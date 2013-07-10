// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

var str : NSString = nil
var url : NSURL = nil

println("\(str == nil) \(nil == url) \(str == url)")
// CHECK: true true true

str = "abc"
url = NSURL(initWithString: "file:///")
println("\(str == nil) \(nil == url)")
// CHECK: false false

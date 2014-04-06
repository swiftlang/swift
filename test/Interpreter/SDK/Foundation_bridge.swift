// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// CHECK: 17 bridges to 17
var i = 17
if let obj: AnyObject = bridgeToObjectiveC(i) {
  println("\(i) bridges to \(obj.description!())")
} else {
  println("\(i) is not bridged to Objective-C")
}

// CHECK: 3.14159 bridges to 3.14159
var d = 3.14159
if let obj: AnyObject = bridgeToObjectiveC(d) {
  println("\(d) bridges to \(obj.description!())")
} else {
  println("\(d) is not bridged to Objective-C")
}

// CHECK: Hello, world! bridges to Hello, world!
var s = "Hello, world!"
if let obj: AnyObject = bridgeToObjectiveC(s) {
  println("\(s) bridges to \(obj.description!())")
} else {
  println("\(s) is not bridged to Objective-C")
}

// CHECK: array bridges to <something unusable>
var a = [1, 2, 3]
if let obj: AnyObject = bridgeToObjectiveC(a) {
  // FIXME: Printing \(obj.description!()) here crashes
  println("array bridges to <something unusable>")
} else {
  println("array is not bridged to Objective-C")
}

// CHECK: dictionary is not bridged to Objective-C
var dict = [1: "Hello", 2: "World"]
if let obj: AnyObject = bridgeToObjectiveC(dict) {
  // FIXME: Printing \(obj.description!()) here 
  println("dictionary bridges to \(obj.description!())")
} else {
  println("dictionary is not bridged to Objective-C")
}

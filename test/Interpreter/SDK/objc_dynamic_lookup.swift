// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// Dynamic subscripting of NSArray, dynamic method dispatch
// CHECK: 3
var array : AnyObject = [1, 2, 3, 4, 5]
println(array[2].description)

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSArray subscript with an object fails
var optVal1 = array["Hello"]
if optVal1 {
   println(optVal1.description)
} else {
   println("NSArray subscript with an object fails")
}

// Dynamic subscripting of NSDictionary, dynamic method dispatch
// CHECK: 2
var nsdict : NSDictionary = ["Hello" : 1, "World" : 2]
var dict : AnyObject = nsdict
println(dict["World"].description)

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSDictionary subscript with an object fails
var optVal2 = dict[1]
if optVal2 {
   println(optVal2.description)
} else {
   println("NSDictionary subscript with an object fails")
}

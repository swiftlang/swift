// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// Dynamic subscripting of NSArray, dynamic method dispatch
// CHECK: 3
var array : AnyObject = [1, 2, 3, 4, 5]
#if os(OSX)
println(array[2].description)
#else
println(array[2]!.description!())
#endif

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSArray subscript with an object fails
var optVal1 = array["Hello"]
if optVal1 {
#if os(OSX)
   println(optVal1.description)
#else
   println(optVal1!.description!())
#endif
} else {
   println("NSArray subscript with an object fails")
}

// Dynamic subscripting of NSDictionary, dynamic method dispatch
// CHECK: 2
var nsdict : NSDictionary = ["Hello" : 1, "World" : 2]
var dict : AnyObject = nsdict
#if os(OSX)
println(dict["World"].description)
#else
println(dict["World"]!.description!())
#endif

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSDictionary subscript with an object fails
var optVal2 = dict[1]
if optVal2 {
#if os(OSX)
   println(optVal2.description)
#else
   println(optVal2!.description!())
#endif
} else {
   println("NSDictionary subscript with an object fails")
}

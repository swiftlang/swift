// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// Dynamic subscripting of NSArray, dynamic method dispatch
// CHECK: {{^3$}}
var array : AnyObject = [1, 2, 3, 4, 5]
print(array[2].description)

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSArray subscript with an object fails
var optVal1 = array["Hello"]
if optVal1 != nil {
   print((optVal1!)!.description)
} else {
   print("NSArray subscript with an object fails")
}

// Dynamic subscripting of NSDictionary, dynamic method dispatch
// CHECK: {{^2$}}
var nsdict : NSDictionary = ["Hello" : 1, "World" : 2]
var dict : AnyObject = nsdict
print((dict["World"]!)!.description)

// Dynamic subscripting on a dictionary using an index (fails)
// CHECK: NSDictionary subscript with an index fails
var optVal2 = dict[1]
if optVal2 != nil {
   print(optVal2.description)
} else {
   print("NSDictionary subscript with an index fails")
}

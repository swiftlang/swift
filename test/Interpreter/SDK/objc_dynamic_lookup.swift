// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

// Dynamic subscripting of NSArray, dynamic method dispatch
// CHECK: {{^3$}}
var array : AnyObject = [1 as NSNumber, 2 as NSNumber, 3 as NSNumber, 4 as NSNumber, 5 as NSNumber] as NSArray
print((array[2] as AnyObject).description)

// Dynamic subscripting on an array using an object (fails)
// CHECK: NSArray subscript with an object fails
var optVal1 = array["Hello" as NSString]
if optVal1 != nil {
   print(((optVal1!)! as AnyObject).description)
} else {
   print("NSArray subscript with an object fails")
}

// Dynamic subscripting of NSDictionary, dynamic method dispatch
// CHECK: {{^2$}}
var nsdict : NSDictionary = ["Hello" as NSString : 1 as NSNumber, "World" as NSString : 2 as NSNumber]
var dict : AnyObject = nsdict
print(((dict["World" as NSString]!)! as AnyObject).description)

// Dynamic subscripting on a dictionary using an index (fails)
// CHECK: NSDictionary subscript with an index fails
var optVal2 = dict[1]
if optVal2 != nil {
   print((optVal2! as AnyObject).description)
} else {
   print("NSDictionary subscript with an index fails")
}

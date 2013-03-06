// RUN: %swift -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//
var str = "Hello"
var nsStr : NSString = str
// FIXME: Comparisons between imported enums
assert(nsStr.compare(str).value == NSOrderedSame.value)
nsStr = "World"
str = nsStr
assert(str == nsStr)

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//
var i = 17
var d = 3.14159
var b = true

// Implicit boxing/explicit unboxing
var nsNum : NSNumber = i
assert(Int(nsNum) == i)

nsNum = d
assert(Double(nsNum) == d)

nsNum = b
assert(Bool(nsNum) == b)

// Literals
nsNum = 42
assert(Int(nsNum) == 42)

nsNum = 3.14159
assert(Double(nsNum) == 3.14159)

nsNum = false
assert(Bool(nsNum) == false)

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

// Literals
var nsArr : NSArray = [1, 2.5, "Hello"]
assert(nsArr.count() == 3)

// Subscripting
assert(Int(NSNumber(nsArr[0])) == 1)
assert(Double(NSNumber(nsArr[1])) == 2.5)
assert(NSString(nsArr[2]).isEqualTo("Hello"))

// Iteration
for x in nsArr { 
  print("Element = \(x.description())\n")
}
// CHECK: Element = 1
// CHECK: Element = 2.5
// CHECK: Element = Hello

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//
var nsDict : NSDictionary = [1 : "Hello", 2 : "World"]
assert(NSString(nsDict[1]).isEqualTo("Hello"))
assert(NSString(nsDict[2]).isEqualTo("World"))

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//
var nsRange : NSRange = 1..5

//===----------------------------------------------------------------------===//
// NSRect
//===----------------------------------------------------------------------===//
var nsRect = NSRect(1, 5, 20, 30)

//===----------------------------------------------------------------------===//
// URLs
//===----------------------------------------------------------------------===//
var nsURL : NSURL = "http://llvm.org"

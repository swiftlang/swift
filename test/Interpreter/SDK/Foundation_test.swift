// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func asNSString(s: String) -> NSString { return s }
func asString(ns: NSString) -> String { return ns }

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//
var str = "Hello"
var nsStr : NSString = str
assert(nsStr.compare(str).toRaw() == NSComparisonResult.OrderedSame.toRaw())
assert(nsStr.compare(str) == NSComparisonResult.OrderedSame)
nsStr = "World"
str = nsStr
// FIXME: Shouldn't need coercion here to resolve ambiguity. <rdar://problem/14637688>
assert(str == asString(nsStr))

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
assert(nsArr.count == 3)

// Subscripting
assert(Int((nsArr[0] as NSNumber)!) == 1)
assert(Double((nsArr[1] as NSNumber)!) == 2.5)
assert((nsArr[2] as NSString)!.isEqual("Hello"))

// Iteration
for x : AnyObject in nsArr { 
  // FIXME: InterpolatedStringExpr fails to type-check with NSString because
  // of ambiguous NSString constructors.
  print("Element = ")
  // FIXME: There is an ambiguity between the class method in NSObject
  // and the property in NSObject which using 'x' as AnyObject.
  // This crashes the type checker.
  print((x as NSObject!).description)
  print("\n")
}
// CHECK: Element = 1
// CHECK: Element = 2.5
// CHECK: Element = Hello

// Mutation
var nsMutableArr : NSMutableArray = ["Constant", "Moon"]
nsMutableArr[0] = "Inconstant"
println("mutable array \(nsMutableArr[0] as NSString!) \(nsMutableArr[1] as NSString!)")
assert(nsMutableArr.count == 2)
// CHECK: mutable array Inconstant Moon

// Construction
var variadicArray = NSArray(objects: "A", "B", "C")
// CHECK: variadic count = 3
print("variadic count = \(variadicArray.count)")

// Coercions
var nsa = NSArray()
var aoa: Array<AnyObject> = []

nsa as Array<AnyObject>
var nsa2 = NSArray()
var aoa2: Array<AnyObject> = nsa2

var nsaoa = aoa as NSArray

func nsArrayToAnyObjectArray(nsa: NSArray) -> AnyObject[] {
  return nsa
}

nsArrayToAnyObjectArray(nsa)
nsArrayToAnyObjectArray(aoa)


//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//
var nsDict : NSDictionary = [1 : "Hello", 2 : "World"]
assert((nsDict[1] as NSString)!.isEqual("Hello"))
assert((nsDict[2] as NSString)!.isEqual("World"))

let nsMutableDict: NSMutableDictionary = ["Hello" : 1, "World" : 2]
assert(nsMutableDict["Hello"].isEqual(1))
assert(nsMutableDict["World"].isEqual(2))

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//
var nsRange = NSRange(1...5)
println(NSStringFromRange(nsRange))
// CHECK: {1, 4}

//===----------------------------------------------------------------------===//
// URLs
//===----------------------------------------------------------------------===//
var nsURL = NSURL(string:"http://llvm.org")
println(nsURL.description)
// CHECK: http://llvm.org


//===----------------------------------------------------------------------===//
// Pattern-matching
//===----------------------------------------------------------------------===//

func matchesEither(input: NSNumber, a: NSNumber, b: NSNumber) -> Bool {
  switch input {
  case a, b:
    return true
  default:
    return false
  }
}

var one, two, three, oneAgain : NSNumber
one = NSNumber(int: 1)
two = NSNumber(int: 2)
three = NSNumber(int: 3)
oneAgain = NSNumber(int: 1)
print(matchesEither(one, two, three))
print(" ")
print(matchesEither(one, oneAgain, three))
print(" ")
print(matchesEither(one, two, oneAgain))
println()
// CHECK: false true true


//===----------------------------------------------------------------------===//
// Miscellaneous
//===----------------------------------------------------------------------===//

// <rdar://problem/14474701>
class ClassWithDtor : NSObject {
  deinit {
    var noteCenter = NSNotificationCenter.defaultCenter()
    noteCenter.removeObserver(self, name: "ReceivedContentNotification", object: nil)
  }
}

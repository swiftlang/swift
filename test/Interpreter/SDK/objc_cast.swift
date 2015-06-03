// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

@objc protocol SwiftObjCProto {}

class SwiftSuperPort : NSPort { }

class SwiftSubPort : SwiftSuperPort { }

class SwiftSuper { }
class SwiftSub : SwiftSuper { }

extension NSPort : SwiftObjCProto {}

var obj : AnyObject

func genericCast<T>(x: AnyObject, _: T.Type) -> T? {
  return x as? T
}

// Test instance of Swift subclass of Objective-C class

obj = SwiftSubPort()
_ = obj as! SwiftSubPort
_ = obj as! SwiftSuperPort
_ = (obj as? NSPort)
_ = (obj as? NSObject)!
if (obj as? SwiftSubPort) == nil    { abort() }
if (obj as? SwiftSuperPort) ==  nil { abort() }
if (obj as? NSPort) == nil          { abort() }
if (obj as? NSObject) == nil        { abort() }
if (obj as? NSArray) != nil         { abort() }
if (obj as? SwiftSub) != nil        { abort() }
if (obj as? SwiftSuper) != nil      { abort() }

obj = SwiftSuperPort()
_ = obj as! SwiftSuperPort
_ = obj as! NSPort
_ = obj as! NSObject
if (obj as? SwiftSubPort) != nil   { abort() }
if (obj as? SwiftSuperPort) == nil { abort() }
if (obj as? NSPort) == nil         { abort() }
if (obj as? NSObject) == nil       { abort() }
if (obj as? NSArray) != nil        { abort() }
if (obj as? SwiftSub) != nil       { abort() }
if (obj as? SwiftSuper) != nil     { abort() }

// Test instance of Objective-C class that has Swift subclass

obj = NSPort()
_ = obj as! NSPort
_ = obj as! NSObject
if (obj as? SwiftSubPort) != nil    { abort() }
if (obj as? SwiftSuperPort) !=  nil { abort() }
if (obj as? NSPort) == nil          { abort() }
if (obj as? NSObject) == nil        { abort() }
if (obj as? NSArray) != nil         { abort() }
if (obj as? SwiftSub) != nil        { abort() }
if (obj as? SwiftSuper) != nil      { abort() }
if (obj as? SwiftObjCProto) == nil  { abort() }

obj = NSPort()
_ = genericCast(obj, NSPort.self)!
_ = genericCast(obj, NSObject.self)!
if genericCast(obj, SwiftSubPort.self) != nil   { abort() }
if genericCast(obj, SwiftSuperPort.self) != nil { abort() }
if genericCast(obj, NSPort.self) == nil         { abort() }
if genericCast(obj, NSObject.self) == nil       { abort() }
if genericCast(obj, NSArray.self) != nil        { abort() }
if genericCast(obj, SwiftSub.self) != nil       { abort() }
if genericCast(obj, SwiftSuper.self) != nil     { abort() }

obj = NSObject()
_ = obj as! NSObject
if  (obj as? SwiftSubPort) != nil   { abort() }
if  (obj as? SwiftSuperPort) != nil { abort() }
if  (obj as? NSPort) != nil         { abort() }
if  (obj as? NSObject) == nil       { abort() }
if  (obj as? NSCopying) != nil      { abort() }
if  (obj as? NSArray) != nil        { abort() }
if  (obj as? SwiftSub) != nil       { abort() }
if  (obj as? SwiftSuper) != nil     { abort() }
if  (obj as? SwiftObjCProto) != nil { abort() }

// Test instance of a tagged pointer type

obj = NSNumber(int: 1234567)
_ = obj as! NSNumber
_ = obj as! NSValue
_ = obj as! NSObject
_ = obj as! NSCopying
if  (obj as? SwiftSubPort) != nil   { abort() }
if  (obj as? SwiftSuperPort) != nil { abort() }
if  (obj as? NSNumber) == nil       { abort() }
if  (obj as? NSValue) == nil        { abort() }
if  (obj as? NSObject) == nil       { abort() }
if  (obj as? NSCopying) == nil      { abort() }
if  (obj as? NSArray) != nil        { abort() }
if  (obj as? SwiftSub) != nil       { abort() }
if  (obj as? SwiftSuper) != nil     { abort() }

// Test instance of a Swift class with no Objective-C inheritance

obj = SwiftSub()
_ = obj as! SwiftSub
_ = obj as! SwiftSuper
if  (obj as? SwiftSubPort) != nil   { abort() }
if  (obj as? SwiftSuperPort) != nil { abort() }
if  (obj as? NSObject) != nil       { abort() }
if  (obj as? NSArray) != nil        { abort() }
if  (obj as? SwiftSub) == nil       { abort() }
if  (obj as? SwiftSuper) == nil     { abort() }

obj = SwiftSuper()
_ = obj as! SwiftSuper
if  (obj as? SwiftSubPort) != nil   { abort() }
if  (obj as? SwiftSuperPort) != nil { abort() }
if  (obj as? NSObject) != nil       { abort() }
if  (obj as? NSArray) != nil        { abort() }
if  (obj as? SwiftSub) != nil       { abort() }
if  (obj as? SwiftSuper) == nil     { abort() }

// Test optional and non-optional bridged conversions
var ao: AnyObject = "s"
ao as! String
ao is String

var auo: AnyObject! = "s"
var s: String = auo as! String

var auoo: AnyObject? = "s"
auoo! as? String

// Test bridged casts.
// CHECK: Downcast to hello
obj = NSString(string: "hello")
if let str = obj as? String {
  print("Downcast to \(str)")
} else {
  print("Not a string?")
}

// Forced cast using context
// CHECK-NEXT: Forced to string hello
let forcedStr: String = obj as! String
print("Forced to string \(forcedStr)")

// CHECK-NEXT: Downcast to Swift
var objOpt: AnyObject? = NSString(string: "Swift")
if let str = objOpt as? String {
  print("Downcast to \(str)")
} else {
  print("Not a string?")
}

// Forced cast using context
// CHECK-NEXT: Forced to string Swift
let forcedStrOpt: String = objOpt as! String
print("Forced to string \(forcedStrOpt)")

// CHECK-NEXT: Downcast to world
var objImplicitOpt: AnyObject! = NSString(string: "world")
if let str = objImplicitOpt as? String {
  print("Downcast to \(str)")
} else {
  print("Not a string?")
}

// Forced cast using context
// CHECK-NEXT: Forced to string world
let forcedStrImplicitOpt: String = objImplicitOpt as! String
print("Forced to string \(forcedStrImplicitOpt)")

// CHECK-NEXT: Downcast correctly failed due to nil
objOpt = nil
if let str = objOpt as? String {
  print("Downcast should not succeed for nil")
} else {
  print("Downcast correctly failed due to nil")
}

// CHECK-NEXT: Downcast correctly failed due to nil
objImplicitOpt = nil
if let str = objImplicitOpt as? String {
  print("Downcast should not succeed for nil")
} else {
  print("Downcast correctly failed due to nil")
}

// Test bridged "isa" checks.
// CHECK: It's a string!
obj = NSString(string: "hello")
if obj is String {
  print("It's a string!")
} else {
  print("Not a string?")
}

// CHECK-NEXT: It's a string!
objOpt = NSString(string: "Swift")
if objOpt is String {
  print("It's a string!")
} else {
  print("Not a string?")
}

// CHECK-NEXT: It's a string!
objImplicitOpt = NSString(string: "world")
if objImplicitOpt is String {
  print("It's a string!")
} else {
  print("Not a string?")
}

// CHECK-NEXT: Isa correctly failed due to nil
objOpt = nil
if objOpt is String {
  print("Isa should not succeed for nil")
} else {
  print("Isa correctly failed due to nil")
}

// CHECK-NEXT: Isa correctly failed due to nil
objImplicitOpt = nil
if objImplicitOpt is String {
  print("Isa should not succeed for nil")
} else {
  print("Isa correctly failed due to nil")
}

let words = ["Hello", "Swift", "World"]

// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
obj = words as AnyObject
if let strArr = obj as? [String] {
  print("Object-to-bridged-array cast produced \(strArr)")
} else {
  print("Object-to-bridged-array cast failed")
}

// Check downcast from the bridged type itself.
// CHECK-NEXT: NSArray-to-bridged-array cast produced [Hello, Swift, World]
var nsarr = words as NSArray
if let strArr = nsarr as? [String] {
  print("NSArray-to-bridged-array cast produced \(strArr)")
} else {
  print("NSArray-to-bridged-array cast failed")
}

// CHECK-NEXT: NSArray?-to-bridged-array cast produced [Hello, Swift, World]
var nsarrOpt = words as NSArray?
if let strArr = nsarrOpt as? [String] {
  print("NSArray?-to-bridged-array cast produced \(strArr)")
} else {
  print("NSArray?-to-bridged-array cast failed")
}

// CHECK-NEXT: NSArray!-to-bridged-array cast produced [Hello, Swift, World]
var nsarrImplicitOpt = words as NSArray!
if let strArr = nsarrImplicitOpt as? [String] {
  print("NSArray!-to-bridged-array cast produced \(strArr)")
} else {
  print("NSArray!-to-bridged-array cast failed")
}

// Check downcast from a superclass of the bridged type.
// CHECK-NEXT: NSObject-to-bridged-array cast produced [Hello, Swift, World]
var nsobj: NSObject = nsarr
if let strArr = nsobj as? [String] {
  print("NSObject-to-bridged-array cast produced \(strArr)")
} else {
  print("NSObject-to-bridged-array cast failed")
}

// CHECK-NEXT: NSArray is [String]
if nsarr is [String] {
  print("NSArray is [String]")
} else {
  print("NSArray is not a [String]")
}

// CHECK-NEXT: NSArray? is [String]
if nsarrOpt is [String] {
  print("NSArray? is [String]")
} else {
  print("NSArray? is not a [String]")
}

// CHECK-NEXT: NSArray! is [String]
if nsarrImplicitOpt is [String] {
  print("NSArray! is [String]")
} else {
  print("NSArray! is not a [String]")
}

// CHECK-NEXT: NSObject is [String]
if nsobj is [String] {
  print("NSObject is [String]")
} else {
  print("NSObject is not a [String]")
}

// Forced downcast based on context.
// CHECK-NEXT: Forced to string array [Hello, Swift, World]
var forcedStrArray: [String] = obj as! [String]
print("Forced to string array \(forcedStrArray)")

// CHECK-NEXT: Forced NSArray to string array [Hello, Swift, World]
forcedStrArray = nsarr as! [String]
print("Forced NSArray to string array \(forcedStrArray)")

// CHECK-NEXT: Forced NSArray? to string array [Hello, Swift, World]
forcedStrArray = nsarrOpt as! [String]
print("Forced NSArray? to string array \(forcedStrArray)")

// CHECK-NEXT: Forced NSArray! to string array [Hello, Swift, World]
forcedStrArray = nsarrImplicitOpt as! [String]
print("Forced NSArray! to string array \(forcedStrArray)")

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = obj as? [NSString] {
  print("Object-to-array cast produced \(strArr)")
} else {
  print("Object-to-array cast failed")
}

// CHECK-NEXT: Object-to-bridged-array cast failed due to bridge mismatch
if let strArr = obj as? [Int] {
  print("Object-to-bridged-array cast should not have succedded")
} else {
  print("Object-to-bridged-array cast failed due to bridge mismatch")
}

// CHECK-NEXT: Array of strings is not an array of ints
if obj is [Int] {
  print("Array of strings should not be an array of ints!")
} else {
  print("Array of strings is not an array of ints")
}

// Implicitly unwrapped optional of object to array casts.
// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
objOpt = words as AnyObject?
if let strArr = objOpt as? [String] {
  print("Object-to-bridged-array cast produced \(strArr)")
} else {
  print("Object-to-bridged-array cast failed")
}

// Forced downcast based on context.
// CHECK-NEXT: Forced to string array [Hello, Swift, World]
let forcedStrArrayOpt: [String] = objOpt as! [String]
print("Forced to string array \(forcedStrArrayOpt)")

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = objOpt as? [NSString] {
  print("Object-to-array cast produced \(strArr)")
} else {
  print("Object-to-array cast failed")
}

// CHECK: Object-to-bridged-array cast failed due to bridge mismatch
if let intArr = objOpt as? [Int] {
  print("Object-to-bridged-array cast should not have succedded")
} else {
  print("Object-to-bridged-array cast failed due to bridge mismatch")
}

// CHECK: Object-to-bridged-array cast failed due to nil
objOpt = nil
if let strArr = objOpt as? [String] {
  print("Cast from nil succeeded?")
} else {
  print("Object-to-bridged-array cast failed due to nil")
}

// Optional of object to array casts.
// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
objImplicitOpt = words as AnyObject!
if let strArr = objImplicitOpt as? [String] {
  print("Object-to-bridged-array cast produced \(strArr)")
} else {
  print("Object-to-bridged-array cast failed")
}

// Forced downcast based on context.
// CHECK-NEXT: Forced to string array [Hello, Swift, World]
let forcedStrArrayImplicitOpt: [String] = objImplicitOpt as! [String]
print("Forced to string array \(forcedStrArrayImplicitOpt)")

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = objImplicitOpt as? [NSString] {
  print("Object-to-array cast produced \(strArr)")
} else {
  print("Object-to-array cast failed")
}

// CHECK: Object-to-bridged-array cast failed due to bridge mismatch
if let intArr = objImplicitOpt as? [Int] {
  print("Object-to-bridged-array cast should not have succedded")
} else {
  print("Object-to-bridged-array cast failed due to bridge mismatch")
}

// CHECK: Object-to-bridged-array cast failed due to nil
objImplicitOpt = nil
if let strArr = objImplicitOpt as? [String] {
  print("Cast from nil succeeded?")
} else {
  print("Object-to-bridged-array cast failed due to nil")
}

// Casting an array of numbers to different numbers.
// CHECK: Numbers-as-doubles cast produces [3.14159, 2.71828, 0.0]
obj = ([3.14159, 2.71828, 0] as [Double]) as AnyObject
if let doubleArr = obj as? [Double] {
  print("Numbers-as-doubles cast produces \(doubleArr)")
} else {
  print("Numbers-as-doubles failed")
}

// CHECK: Numbers-as-floats cast produces [3.14159{{.*}}, 2.71828{{.*}}, 0.0]
if let floatArr = obj as? [Float] {
  print("Numbers-as-floats cast produces \(floatArr)")
} else {
  print("Numbers-as-floats failed")
}

// CHECK: Numbers-as-ints cast produces [3, 2, 0]
if let intArr = obj as? [Int] {
  print("Numbers-as-ints cast produces \(intArr)")
} else {
  print("Numbers-as-ints failed")
}

// CHECK: Numbers-as-bools cast produces [true, true, false]
if let boolArr = obj as? [Bool] {
  print("Numbers-as-bools cast produces \(boolArr)")
} else {
  print("Numbers-as-bools failed")
}

class Base : NSObject { 
  override var description: String {
    return "Base"
  }
}
class Derived : Base { 
  override var description: String {
    return "Derived"
  }
}

// CHECK: Array-of-base cast produces [Derived, Derived, Base]
obj = [Derived(), Derived(), Base()]
if let baseArr = obj as? [Base] {
  print("Array-of-base cast produces \(baseArr)")
} else {
  print("Not an array of base")
}

// CHECK: Not an array of derived
if let derivedArr = obj as? [Derived] {
  print("Array-of-derived cast produces \(derivedArr)")
} else {
  print("Not an array of derived")
}

// CHECK: Dictionary-of-base-base cast produces
obj = [Derived() : Derived(), Derived() : Base(), Derived() : Derived() ] as AnyObject
if let baseDict = obj as? Dictionary<Base, Base> {
  print("Dictionary-of-base-base cast produces \(baseDict)")
} else {
  print("Not a dictionary of base/base")
}

// CHECK: Dictionary-of-derived-base cast produces
if let baseDict = obj as? Dictionary<Derived, Base> {
  print("Dictionary-of-derived-base cast produces \(baseDict)")
} else {
  print("Not a dictionary of derived/base")
}

// CHECK: Not a dictionary of derived/derived
if let dict = obj as? Dictionary<Derived, Derived> {
  print("Dictionary-of-derived-derived cast produces \(dict)")
} else {
  print("Not a dictionary of derived/derived")
}

let strArray: AnyObject = ["hello", "world"]
let intArray: AnyObject = [1, 2, 3]
let dictArray: AnyObject = [["hello" : 1, "world" : 2], 
                            ["swift" : 1, "speedy" : 2]]

// CHECK: Dictionary<String, AnyObject> is
obj = ["a" : strArray, "b" : intArray, "c": dictArray]
if let dict = obj as? Dictionary<String, [AnyObject]> {
  print("Dictionary<String, AnyObject> is \(dict)")
} else {
  print("Not a Dictionary<String, AnyObject>")
}

// CHECK: Not a Dictionary<String, String>
if let dict = obj as? Dictionary<String, [String]> {
  print("Dictionary<String, String> is \(dict)")
} else {
  print("Not a Dictionary<String, String>")
}

// CHECK: Not a Dictionary<String, Int>
if let dict = obj as? Dictionary<String, [Int]> {
  print("Dictionary<String, Int> is \(dict)")
} else {
  print("Not a Dictionary<String, Int>")
}

// CHECK: [Dictionary<String, Int>] is 
obj = dictArray
if let array = obj as? [Dictionary<String, Int>] {
  print("[Dictionary<String, Int>] is \(array)")
} else {
  print("Not a [Dictionary<String, Int>]")
}

// CHECK: Not a [Dictionary<String, String>]
if let array = obj as? [Dictionary<String, String>] {
  print("[Dictionary<String, String>] is \(array)")
} else {
  print("Not a [Dictionary<String, String>]")
}

// CHECK: Dictionary<String, [Dictionary<String, Int>]> is [a: [
obj = ["a" : dictArray]
if let dict = obj as? Dictionary<String, [Dictionary<String, Int>]> {
  print("Dictionary<String, [Dictionary<String, Int>]> is \(dict)")
} else {
  print("Not a Dictionary<String, [Dictionary<String, Int>]>")
}

// CHECK: Not a Dictionary<String, [Dictionary<String, String>]>
if let dict = obj as? Dictionary<String, [Dictionary<String, String>]> {
  print("Dictionary<String, [Dictionary<String, String>]> is \(dict)")
} else {
  print("Not a Dictionary<String, [Dictionary<String, String>]>")
}

// CHECK: [Dictionary<String, [Dictionary<String, Int>]>] is
obj = [obj, obj, obj]
if let array = obj as? [Dictionary<String, [Dictionary<String, Int>]>] {
  print("[Dictionary<String, [Dictionary<String, Int>]>] is \(array)")
} else {
  print("Not a [Dictionary<String, [Dictionary<String, Int>]>]")
}

// CHECK: Not a Dictionary<String, [Dictionary<String, String>]>[]
if let array = obj as? Dictionary<String, [Dictionary<String, String>]> {
  print("Dictionary<String, [Dictionary<String, String>]>[] is \(array)")
} else {
  print("Not a Dictionary<String, [Dictionary<String, String>]>[]")
}

// Helper function that downcasts 
func downcastToStringArrayOptOpt(obj: AnyObject??!!) {
  if let strArrOptOpt = obj as? [String]?? {
    if let strArrOpt = strArrOptOpt {
      if let strArr = strArrOpt {
        print("some(some(some(\(strArr))))")
      } else {
        print("some(some(none))")
      }
    } else {
      print("some(none)")
    }
  } else {
    print("none")
  }
}

// CHECK: {{^}}some(some(some([a, b, c]))){{$}}
var objOptOpt: AnyObject?? = .Some(.Some(["a", "b", "c"]))
downcastToStringArrayOptOpt(objOptOpt)

// CHECK: {{^}}none{{$}}
objOptOpt = .Some(.Some([1 : "hello", 2 : "swift", 3 : "world"]))
downcastToStringArrayOptOpt(objOptOpt)

// CHECK: {{^}}none{{$}}
objOptOpt = .Some(.Some([1, 2, 3]))
downcastToStringArrayOptOpt(objOptOpt)

print("ok")  // CHECK: ok

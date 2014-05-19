// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

@objc @class_protocol protocol SwiftObjCProto {}

class SwiftSuperPort : NSPort { }

class SwiftSubPort : SwiftSuperPort { }

class SwiftSuper { }
class SwiftSub : SwiftSuper { }

extension NSPort : SwiftObjCProto {}

var obj : AnyObject

func genericCast<T>(x: AnyObject, _: T.Type) -> T? {
  return x as T
}

// Test instance of Swift subclass of Objective-C class

obj = SwiftSubPort()
_ = (obj as SwiftSubPort)!
_ = (obj as SwiftSuperPort)!
_ = (obj as NSPort)
_ = (obj as NSObject)!
if !(obj as SwiftSubPort)   { abort() }
if !(obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

obj = SwiftSuperPort()
_ = (obj as SwiftSuperPort)!
_ = (obj as NSPort)!
_ = (obj as NSObject)!
if  (obj as SwiftSubPort)   { abort() }
if !(obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

// Test instance of Objective-C class that has Swift subclass

obj = NSPort()
_ = (obj as NSPort)!
_ = (obj as NSObject)!
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }
if !(obj as SwiftObjCProto) { abort() }

obj = NSPort()
_ = genericCast(obj, NSPort.self)!
_ = genericCast(obj, NSObject.self)!
if  genericCast(obj, SwiftSubPort.self)   { abort() }
if  genericCast(obj, SwiftSuperPort.self) { abort() }
if !genericCast(obj, NSPort.self)         { abort() }
if !genericCast(obj, NSObject.self)       { abort() }
if  genericCast(obj, NSArray.self)        { abort() }
if  genericCast(obj, SwiftSub.self)       { abort() }
if  genericCast(obj, SwiftSuper.self)     { abort() }

obj = NSObject()
_ = (obj as NSObject)!
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSCopying)      { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }
if  (obj as SwiftObjCProto) { abort() }

// Test instance of a tagged pointer type

obj = NSNumber(int: 1234567)
_ = (obj as NSNumber)!
_ = (obj as NSValue)!
_ = (obj as NSObject)!
_ = (obj as NSCopying)!
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if !(obj as NSNumber)       { abort() }
if !(obj as NSValue)        { abort() }
if !(obj as NSObject)       { abort() }
if !(obj as NSCopying)      { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

// Test instance of a Swift class with no Objective-C inheritance

obj = SwiftSub()
_ = (obj as SwiftSub)!
_ = (obj as SwiftSuper)!
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if !(obj as SwiftSub)       { abort() }
if !(obj as SwiftSuper)     { abort() }

obj = SwiftSuper()
_ = (obj as SwiftSuper)!
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if !(obj as SwiftSuper)     { abort() }

// Test optional and non-optional bridged conversions
var ao: AnyObject = "s"
ao as String
ao is String

var auo: AnyObject! = "s"
var s: String = auo!

var auoo: AnyObject? = "s"
auoo! as String

// Test bridged casts.
// CHECK: Downcast to hello
obj = NSString(string: "hello")
if let str = obj as String {
  println("Downcast to \(str)")
} else {
  println("Not a string?")
}

// CHECK-NEXT: Downcast to Swift
var objOpt: AnyObject? = NSString(string: "Swift")
if let str = objOpt as String {
  println("Downcast to \(str)")
} else {
  println("Not a string?")
}

// CHECK-NEXT: Downcast to world
var objImplicitOpt: AnyObject! = NSString(string: "world")
if let str = objImplicitOpt as String {
  println("Downcast to \(str)")
} else {
  println("Not a string?")
}


// CHECK-NEXT: Downcast correctly failed due to nil
objOpt = nil
if let str = objOpt as String {
  println("Downcast should not succeed for nil")
} else {
  println("Downcast correctly failed due to nil")
}

// CHECK-NEXT: Downcast correctly failed due to nil
objImplicitOpt = nil
if let str = objImplicitOpt as String {
  println("Downcast should not succeed for nil")
} else {
  println("Downcast correctly failed due to nil")
}

// Test bridged "isa" checks.
// CHECK: It's a string!
obj = NSString(string: "hello")
if obj is String {
  println("It's a string!")
} else {
  println("Not a string?")
}

// CHECK-NEXT: It's a string!
objOpt = NSString(string: "Swift")
if objOpt is String {
  println("It's a string!")
} else {
  println("Not a string?")
}

// CHECK-NEXT: It's a string!
objImplicitOpt = NSString(string: "world")
if objImplicitOpt is String {
  println("It's a string!")
} else {
  println("Not a string?")
}


// CHECK-NEXT: Isa correctly failed due to nil
objOpt = nil
if objOpt is String {
  println("Isa should not succeed for nil")
} else {
  println("Isa correctly failed due to nil")
}

// CHECK-NEXT: Isa correctly failed due to nil
objImplicitOpt = nil
if objImplicitOpt is String {
  println("Isa should not succeed for nil")
} else {
  println("Isa correctly failed due to nil")
}

// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
obj = ["Hello", "Swift", "World"] as String[]
if let strArr = obj as String[] {
  println("Object-to-bridged-array cast produced \(strArr)")
} else {
  println("Object-to-bridged-array cast failed")
}

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = obj as NSString[] {
  println("Object-to-array cast produced \(strArr)")
} else {
  println("Object-to-array cast failed")
}

// CHECK: Object-to-bridged-array cast failed due to bridge mismatch
if let strArr = obj as Int[] {
  println("Object-to-bridged-array cast should not have succedded")
} else {
  println("Object-to-bridged-array cast failed due to bridge mismatch")
}

// Implicitly unwrapped optional of object to array casts.
// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
objOpt = ["Hello", "Swift", "World"] as String[]
if let strArr = objOpt as String[] {
  println("Object-to-bridged-array cast produced \(strArr)")
} else {
  println("Object-to-bridged-array cast failed")
}

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = objOpt as NSString[] {
  println("Object-to-array cast produced \(strArr)")
} else {
  println("Object-to-array cast failed")
}

// CHECK: Object-to-bridged-array cast failed due to bridge mismatch
if let intArr = objOpt as Int[] {
  println("Object-to-bridged-array cast should not have succedded")
} else {
  println("Object-to-bridged-array cast failed due to bridge mismatch")
}

// CHECK: Object-to-bridged-array cast failed due to nil
objOpt = nil
if let strArr = objOpt as String[] {
  println("Cast from nil succeeded?")
} else {
  println("Object-to-bridged-array cast failed due to nil")
}

// Optional of object to array casts.
// CHECK-NEXT: Object-to-bridged-array cast produced [Hello, Swift, World]
objImplicitOpt = ["Hello", "Swift", "World"] as String[]
if let strArr = objImplicitOpt as String[] {
  println("Object-to-bridged-array cast produced \(strArr)")
} else {
  println("Object-to-bridged-array cast failed")
}

// CHECK-NEXT: Object-to-array cast produced [Hello, Swift, World]
if let strArr = objImplicitOpt as NSString[] {
  println("Object-to-array cast produced \(strArr)")
} else {
  println("Object-to-array cast failed")
}

// CHECK: Object-to-bridged-array cast failed due to bridge mismatch
if let intArr = objImplicitOpt as Int[] {
  println("Object-to-bridged-array cast should not have succedded")
} else {
  println("Object-to-bridged-array cast failed due to bridge mismatch")
}

// CHECK: Object-to-bridged-array cast failed due to nil
objImplicitOpt = nil
if let strArr = objImplicitOpt as String[] {
  println("Cast from nil succeeded?")
} else {
  println("Object-to-bridged-array cast failed due to nil")
}

// Casting an array of numbers to different numbers.
// CHECK: Numbers-as-doubles cast produces [3.14159, 2.71828, 0.0]
obj = [3.14159, 2.71828, 0] as Double[]
if let doubleArr = obj as Double[] {
  println("Numbers-as-doubles cast produces \(doubleArr)")
} else {
  println("Numbers-as-doubles failed")
}

// CHECK: Numbers-as-floats cast produces [3.14159{{.*}}, 2.71828{{.*}}, 0.0]
if let floatArr = obj as Float[] {
  println("Numbers-as-floats cast produces \(floatArr)")
} else {
  println("Numbers-as-floats failed")
}


// CHECK: Numbers-as-ints cast produces [3, 2, 0]
if let intArr = obj as Int[] {
  println("Numbers-as-ints cast produces \(intArr)")
} else {
  println("Numbers-as-ints failed")
}

// CHECK: Numbers-as-bools cast produces [true, true, false]
if let boolArr = obj as Bool[] {
  println("Numbers-as-bools cast produces \(boolArr)")
} else {
  println("Numbers-as-bools failed")
}

println("ok")  // CHECK: ok

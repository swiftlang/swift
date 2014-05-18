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

println("ok")  // CHECK: ok

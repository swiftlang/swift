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

println("ok")  // CHECK: ok

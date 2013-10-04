// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -module-cache-path=%t/clang-module-cache -sdk=%sdk %s | FileCheck %s
// REQUIRES: sdk
// REQUIRES: swift_interpreter

import Foundation

class SwiftSuperPort : NSPort { }

class SwiftSubPort : SwiftSuperPort { }

class SwiftSuper { }
class SwiftSub : SwiftSuper { }

var obj : id

// Test instance of Swift subclass of Objective-C class

obj = SwiftSubPort()
_ = obj as! SwiftSubPort
_ = obj as! SwiftSuperPort
_ = obj as! NSPort
_ = obj as! NSObject
if !(obj as SwiftSubPort)   { abort() }
if !(obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

obj = SwiftSuperPort()
_ = obj as! SwiftSuperPort
_ = obj as! NSPort
_ = obj as! NSObject
if  (obj as SwiftSubPort)   { abort() }
if !(obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

// Test instance of Objective-C class that has Swift subclass

obj = NSPort()
_ = obj as! NSPort
_ = obj as! NSObject
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if !(obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

obj = NSObject()
_ = obj as! NSObject
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSPort)         { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

// Test instance of a tagged pointer type

obj = NSNumber.numberWithInt(1234567)
_ = obj as! NSNumber
_ = obj as! NSValue
_ = obj as! NSObject
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if !(obj as NSNumber)       { abort() }
if !(obj as NSValue)        { abort() }
if !(obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if  (obj as SwiftSuper)     { abort() }

// Test instance of a Swift class with no Objective-C inheritance

obj = SwiftSub()
_ = obj as! SwiftSub
_ = obj as! SwiftSuper
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if !(obj as SwiftSub)       { abort() }
if !(obj as SwiftSuper)     { abort() }

obj = SwiftSuper()
_ = obj as! SwiftSuper
if  (obj as SwiftSubPort)   { abort() }
if  (obj as SwiftSuperPort) { abort() }
if  (obj as NSObject)       { abort() }
if  (obj as NSArray)        { abort() }
if  (obj as SwiftSub)       { abort() }
if !(obj as SwiftSuper)     { abort() }


println("ok")  // CHECK: ok

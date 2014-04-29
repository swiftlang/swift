// RUN: %target-run-simple-swift

// FIXME: rdar://15931346
// XFAIL: *

import Foundation

struct Guts {
  var internalValue = 42
  var value: Int { 
    get {
      return internalValue
    }
  }
}

class Target : NSString {
  // This ObjC-typed property is observed by KVO
  var objcValue: String

  // This Swift-typed property causes vtable usage on this class.
  var swiftValue: Guts

  init() {
    self.swiftValue = Guts()
    self.objcValue = ""
    super.init()
  }

  func print() { 
    println("swiftValue \(self.swiftValue.value), objcValue \(objcValue)")
  }
}

typealias KVOContext = CMutableVoidPointer

class Observer : NSObject {
  var target: Target?

  init() { target = nil; super.init() }

  func observeTarget(t: Target) 
  {
    target = t
    target!.addObserver(self, forKeyPath:"objcValue", 
      options:NSKeyValueObservingOptions.New | NSKeyValueObservingOptions.Old, 
      context: nil)
  }

  override func observeValueForKeyPath(_ path:String, ofObject obj:AnyObject, 
    change change:NSDictionary, context context:KVOContext) 
  {
    target!.print()
  }
}


var t = Target()
var o = Observer()
println("unobserved")
// CHECK: unobserved
t.objcValue = "one"
t.objcValue = "two"
println("registering observer")
// CHECK-NEXT: registering observer
o.observeTarget(t)
println("Now witness the firepower of this fully armed and operational panopticon!")
// CHECK-NEXT: panopticon
t.objcValue = "three"
// CHECK-NEXT: swiftValue 42, objcValue three
t.objcValue = "four"
// CHECK-NEXT: swiftValue 42, objcValue four

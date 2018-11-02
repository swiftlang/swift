// RUN: %target-run-simple-swift-swift3 | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

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
  // This dynamic property is observed by KVO
  dynamic var objcValue: String

  // This Swift-typed property causes vtable usage on this class.
  var swiftValue: Guts

  override init() {
    self.swiftValue = Guts()
    self.objcValue = ""
    super.init()
  }

  required init?(coder aDecoder: NSCoder) {
    self.swiftValue = Guts()
    self.objcValue = ""
    super.init(coder: aDecoder)
  }
  required init(itemProviderData data: Data, typeIdentifier: String) throws {
    fatalError("don't call this initializer")
  }

  func print() { 
    Swift.print("swiftValue \(self.swiftValue.value), objcValue \(objcValue)")
  }
}

class Observer : NSObject {
  var target: Target?

  override init() { target = nil; super.init() }

  func observeTarget(_ t: Target) {
    target = t
    target!.addObserver(self, forKeyPath:"objcValue",
      options: [.new, .old],
      context: nil)
  }

  override func observeValue(forKeyPath: String?,
                             of obj: Any?,
                             change: Dictionary<NSKeyValueChangeKey, Any>?,
                             context: UnsafeMutableRawPointer?) {
    target!.print()
  }
}


var t = Target()
var o = Observer()
print("unobserved")
// CHECK: unobserved
t.objcValue = "one"
t.objcValue = "two"
print("registering observer")
// CHECK-NEXT: registering observer
o.observeTarget(t)
print("Now witness the firepower of this fully armed and operational panopticon!")
// CHECK-NEXT: panopticon
t.objcValue = "three"
// CHECK-NEXT: swiftValue 42, objcValue three
t.objcValue = "four"
// CHECK-NEXT: swiftValue 42, objcValue four

//===----------------------------------------------------------------------===//
// Test using a proper global context reference.
//===----------------------------------------------------------------------===//

var kvoContext = Int()

class ObserverKVO : NSObject {
  var target: Target?

  override init() { target = nil; super.init() }

  func observeTarget(_ target: Target) {
    self.target = target
    self.target!.addObserver(self,
       forKeyPath: "objcValue",
       options: [.new, .old],
       context: &kvoContext)
  }
  
  func removeTarget() {
    self.target!.removeObserver(self, forKeyPath:"objcValue",
                                      context: &kvoContext)
  }

  override func observeValue(forKeyPath: String?,
                             of obj: Any?,
                             change: Dictionary<NSKeyValueChangeKey, Any>?,
                             context: UnsafeMutableRawPointer?) {
    if context == &kvoContext {
      target!.print()
    }
  }
}


var t2 = Target()
var o2 = ObserverKVO()
print("unobserved 2")
t2.objcValue = "one"
t2.objcValue = "two"
print("registering observer 2")
o2.observeTarget(t2)
print("Now witness the firepower of this fully armed and operational panopticon!")
t2.objcValue = "three"
t2.objcValue = "four"
o2.removeTarget()
print("target removed")

// CHECK: registering observer 2
// CHECK-NEXT: Now witness the firepower of this fully armed and operational panopticon!
// CHECK-NEXT: swiftValue 42, objcValue three
// CHECK-NEXT: swiftValue 42, objcValue four
// CHECK-NEXT: target removed

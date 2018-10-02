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
    init(value: Int) {
        internalValue = value
    }
    init() {
    }
}

class Target : NSObject, NSKeyValueObservingCustomization {
    // This dynamic property is observed by KVO
    dynamic var objcValue: String
    dynamic var objcValue2: String {
        willSet {
            willChangeValue(for: \.objcValue2)
        }
        didSet {
            didChangeValue(for: \.objcValue2)
        }
    }
    dynamic var objcValue3: String
    
    // This Swift-typed property causes vtable usage on this class.
    var swiftValue: Guts
    
    override init() {
        self.swiftValue = Guts()
        self.objcValue = ""
        self.objcValue2 = ""
        self.objcValue3 = ""
        super.init()
    }
    
    static func keyPathsAffectingValue(for key: AnyKeyPath) -> Set<AnyKeyPath> {
        if (key == \Target.objcValue) {
            return [\Target.objcValue2, \Target.objcValue3]
        } else {
            return []
        }
    }
    
    static func automaticallyNotifiesObservers(for key: AnyKeyPath) -> Bool {
        if key == \Target.objcValue2 || key == \Target.objcValue3 {
            return false
        }
        return true
    }
    
    func print() {
        Swift.print("swiftValue \(self.swiftValue.value), objcValue \(objcValue)")
    }
}


class ObserverKVO : NSObject {
    var target: Target?
    var observation: NSKeyValueObservation? = nil
    
    override init() { target = nil; super.init() }
    
    func observeTarget(_ target: Target) {
        self.target = target
        observation = target.observe(\.objcValue) { (object, change) in
            Swift.print("swiftValue \(object.swiftValue.value), objcValue \(object.objcValue)")
        }
    }
    
    func removeTarget() {
        observation!.invalidate()
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
t2.swiftValue = Guts(value: 13)
t2.objcValue2 = "six" //should fire
t2.objcValue3 = "nothing" //should not fire
o2.removeTarget()
t2.objcValue = "five" //make sure that we don't crash or keep posting changes if you deallocate an observation after invalidating it
print("target removed")

// CHECK: registering observer 2
// CHECK-NEXT: Now witness the firepower of this fully armed and operational panopticon!
// CHECK-NEXT: swiftValue 42, objcValue three
// CHECK-NEXT: swiftValue 42, objcValue four
// CHECK-NEXT: swiftValue 13, objcValue four
// CHECK-NEXT: swiftValue 13, objcValue four
// CHECK-NEXT: swiftValue 13, objcValue four
// CHECK-NEXT: target removed


// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

protocol P: class { }

class AbstractP: P { }

class DelegatedP<D: P>: AbstractP {
    init(_ d: D) { }
}

class AnyP: DelegatedP<AbstractP> {
  init<D: P>(_ d: D) {
    super.init(DelegatedP<D>(d))
  }
}

extension P {
  var anyP: AnyP {
    return AnyP(self)
  }
}

// SR-4363
// Test several paths through swift_getObjectType() 

// instance of a Swift class that conforms to P
class O: NSObject, P { }
var o = O()
let obase: NSObject = o
print(NSStringFromClass(object_getClass(o)!))
_ = (o as P).anyP
_ = (obase as! P).anyP
// CHECK: {{^}}main.O{{$}}

// ... and KVO's artificial subclass thereof
o.addObserver(NSObject(), forKeyPath: "xxx", options: [.new], context: nil)
print(NSStringFromClass(object_getClass(o)!))
_ = (o as P).anyP
_ = (obase as! P).anyP
// CHECK-NEXT: NSKVONotifying_main.O

// instance of an ObjC class that conforms to P
extension NSLock: P { }
var l = NSLock()
let lbase: NSObject = l
print(NSStringFromClass(object_getClass(l)!))
_ = (l as P).anyP
_ = (lbase as! P).anyP
// CHECK-NEXT: NSLock

// ... and KVO's artificial subclass thereof
l.addObserver(NSObject(), forKeyPath: "xxx", options: [.new], context: nil)
print(NSStringFromClass(object_getClass(l)!))
_ = (l as P).anyP
_ = (lbase as! P).anyP
// CHECK-NEXT: NSKVONotifying_NSLock

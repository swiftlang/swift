// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

class A : NSObject {
    func a1() {} // never overridden
    func x() {} // overriden 1x
    func y() {} // overriden 2x
    func z() {} // overriden 3x
}

class B : A {
    func b1() {} // never overridden
    override func x() {}
    override func y() {}
    override func z() {}
}

class C : B {
    func c1() {} // never overridden
    override func y() {}
    override func z() {}
}

class D : C {
    @objc let name: String = ""
    func d1() {}
    override func z() {}
}


let o = D()
let observer = NSObject()
o.addObserver(observer, forKeyPath: "name", options: NSKeyValueObservingOptions(), context: nil)

o.x()
o.y()
o.z()
o.a1()
o.b1()
o.c1()
o.d1()

print("okay")
// CHECK: okay

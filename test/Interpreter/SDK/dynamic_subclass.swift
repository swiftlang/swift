// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import ObjectiveC

func DoSwizzle(_ c: AnyClass) -> AnyClass {
    let name = String(utf8String: class_getName(c))!
    let subclass: AnyClass = objc_allocateClassPair(c, "\(name)Subclass", 0)!
    objc_registerClassPair(subclass);
    let subclassSubclass: AnyClass = objc_allocateClassPair(subclass, "\(name)SubclassSubclass", 0)!
    objc_registerClassPair(subclassSubclass);
    return subclassSubclass
}

class MySwiftClassToBeSwizzled: NSObject {
}

_ = DoSwizzle(NSArray.self)
print("Swizzled NSArray")
// CHECK: Swizzled NSArray

_ = DoSwizzle(MySwiftClassToBeSwizzled.self)
print("Swizzled MySwiftClassToBeSwizzled")
// CHECK: Swizzled MySwiftClassToBeSwizzled

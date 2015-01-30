// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let classes = NSMutableArray()
classes.addObject(NSObject.self)
classes.addObject(NSString.self)
classes.addObject(NSNumber.self)

for obj: AnyObject in classes {
  println(obj.description)
}
// CHECK: NSObject
// CHECK-NEXT: NSString
// CHECK-NEXT: NSNumber

println(NSObject.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: false
println(NSString.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: true

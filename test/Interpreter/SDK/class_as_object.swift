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
  print(obj.description)
}
// CHECK: NSObject
// CHECK-NEXT: NSString
// CHECK-NEXT: NSNumber

print(NSObject.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: false
print(NSString.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: true

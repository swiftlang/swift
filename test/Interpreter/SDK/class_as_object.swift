// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

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

// <rdar://problem/17303759> The Protocol class object is hidden on 64-bit iOS,
// so we cannot form its metadata.
#if os(iOS)
println("false\ntrue")
#else
println(NSObject.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: false
println(NSString.conformsToProtocol(NSCopying.self))
// CHECK-NEXT: true
#endif

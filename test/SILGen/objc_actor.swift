// RUN: %target-swift-frontend -emit-silgen %s -swift-version 5  -target %target-swift-5.1-abi-triple | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar://80863853 - For an actor inheriting from NSObject and using '@objc'
// should have the same effect: the effective superclass is SwiftNativeNSObject
// (see 945011d39f8b271b8906bd509aac3aa954f4fc57) not NSObject.
// Check that we don't treat any case as an ObjC class.

import Foundation

public actor MyClass1: NSObject {
  public var x: Int
  public init(_ x: Int) { self.x = x }
}

// CHECK: alloc_ref $MyClass1
// CHECK-NOT: alloc_ref [objc] $MyClass1

@objc public actor MyClass2 {
  public var x: Int
  public init(_ x: Int) { self.x = x }
}

// CHECK: alloc_ref $MyClass2
// CHECK-NOT: alloc_ref [objc] $MyClass2

@objc public actor MyClass3: NSObject {
  public var x: Int
  public init(_ x: Int) { self.x = x }
}

// CHECK: alloc_ref $MyClass3
// CHECK-NOT: alloc_ref [objc] $MyClass3

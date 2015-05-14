// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

class Foo: NSManagedObject {
  // -- POD types:

  // nonatomic, readonly, ivar b
  // CHECK: private unnamed_addr constant {{.*}} c"Tq,N,R,Va\00"
  let a: Int = 0
  // nonatomic, ivar b
  // CHECK: private unnamed_addr constant {{.*}} c"Tq,N,Vb\00"
  var b: Int = 0
  // nonatomic, readonly
  // CHECK: private unnamed_addr constant {{.*}} c"Tq,N,R\00"
  var c: Int { return 0 }
  // nonatomic, assign
  // CHECK: private unnamed_addr constant {{.*}} c"Tq,N\00"
  var d: Int { get { return 0 } set {} }
  // nonatomic, dynamic
  // CHECK: private unnamed_addr constant {{.*}} c"Tq,N,D\00"
  @NSManaged var e: Int

  // -- Class types:
  // TODO: These should use the elaborated @"ClassName" encoding.

  // nonatomic, retain, ivar f
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSData\22,N,&,Vf\00"
  var f: NSData = NSData()
  // nonatomic, weak, assign, ivar g
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSData\22,N,W,Vg\00"
  weak var g: NSData? = nil
  // nonatomic, copy, ivar h
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSData\22,N,C,Vh\00"
  @NSCopying var h: NSData! = nil
  // nonatomic, dynamic, assign
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSData\22,N,D,&\00"
  @NSManaged var i: NSData
  // nonatomic, readonly
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSData\22,N,R\00"
  var j: NSData { return NSData() }

  // -- Bridged value types

  // nonatomic, copy, ivar k
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSString\22,N,C,Vk\00"
  var k: String = ""
  // nonatomic, readonly, ivar l
  // CHECK: private unnamed_addr constant {{.*}} c"T@\22NSString\22,N,R,Vl\00"
  let l: String? = nil
}

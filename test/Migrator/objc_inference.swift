// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -update-code -warn-swift3-objc-inference -primary-file %s -emit-migrated-file-path %t/migrated_objc_inference.swift -o %t/objc_inference.remap
// RUN: not diff -u %s %t/migrated_objc_inference.swift > %t/objc_inference.diff
// RUN: %FileCheck %s < %t/objc_inference.diff
// REQUIRES: objc_interop

import Foundation

class MyClass : NSObject {
  var property : NSObject? = nil
  // CHECK: +  @objc var property : NSObject? = nil

  dynamic var member : Int { return 2 }
  // CHECK: +  @objc dynamic var member : Int { return 2 }

  func foo() {}
  // CHECK: +  @objc func foo() {}

  func baz() {}
  // CHECK: +  @objc func baz() {}
}

extension MyClass {
  func bar() {}
  // CHECK: +  @objc func bar() {}
}

class MySubClass : MyClass {
  override func foo() {}
  override func bar() {}
}

func test(object: AnyObject, mine: MyClass) {
  _ = #selector(MyClass.foo)
  _ = #selector(getter: MyClass.member)
  _ = #keyPath(MyClass.property)
  _ = object.baz?()
}

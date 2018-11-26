// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps
// RUN: %FileCheck %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=DUPLICATE %s < %t.swiftdeps

// Check that the output is deterministic.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t-2.swiftdeps
// RUN: diff %t.swiftdeps %t-2.swiftdeps

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: provides-dynamic-lookup:
// DUPLICATE-LABEL: provides-dynamic-lookup:

@objc @objcMembers class Base : NSObject {
  // CHECK-DAG: - "foo"
  func foo() {}

  // CHECK-DAG: - "bar"
  // DUPLICATE-NOT: "bar"
  // DUPLICATE: - "bar"
  // DUPLICATE-NOT: "bar"
  func bar(_ x: Int, y: Int) {}
  func bar(_ str: String) {}
    
  // CHECK-DAG: - "prop"
  var prop: String?

  // CHECK-DAG: - "unusedProp"
  var unusedProp: Int = 0
  
  // CHECK-DAG: - "classFunc"
  class func classFunc() {}
}

func getAnyObject() -> AnyObject? { return nil }

// DUPLICATE: {{^(provides|depends).+:$}}
// CHECK-LABEL: depends-dynamic-lookup:

func testDynamicLookup(_ obj: AnyObject) {
  // CHECK-DAG: - !private "foo"
  obj.foo()
  // CHECK-DAG: - !private "bar"
  obj.bar(1, y: 2)
  obj.bar("abc")
  
  // CHECK-DAG: - !private "classFunc"
  type(of: obj).classFunc()
  
  // CHECK-DAG: - !private "prop"
  _ = obj.prop
  // CHECK-DAG: - !private "description"
  _ = obj.description
  // CHECK-DAG: - !private "method"
  _ = obj.method(5, with: 5.0 as Double)
  
  // TODO: unable to resolve ambiguity
  // C/HECK-DAG: - !private "subscript"
  // _ = obj[2] as Any
  // _ = obj[2] as Any!
}

// CHECK-DAG: - "counter"
let globalUse = getAnyObject()?.counter

// NEGATIVE-LABEL: depends-dynamic-lookup:
// NEGATIVE-NOT: "cat1Method"
// NEGATIVE-NOT: "unusedProp"

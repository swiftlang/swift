// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

// Check that the output is deterministic.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t-2.swiftdeps
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t-2.swiftdeps %t-2-processed.swiftdeps
// RUN: diff %t-processed.swiftdeps %t-2-processed.swiftdeps

// RUN: %FileCheck %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=DUPLICATE %s < %t-processed.swiftdeps


// REQUIRES: objc_interop

import Foundation

@objc @objcMembers class Base : NSObject {
  // CHECK-DAG:  dynamicLookup implementation  '' foo true
  // CHECK-DAG:  dynamicLookup interface  '' foo true
  func foo() {}

  // CHECK-DAG:  dynamicLookup implementation  '' bar true
  // CHECK-DAG:  dynamicLookup interface  '' bar true

  // DUPLICATE-NOT:  dynamicLookup implementation  '' bar true
  // DUPLICATE:  dynamicLookup implementation  '' bar true
  // DUPLICATE-NOT:  dynamicLookup implementation  '' bar true
  // DUPLICATE-NOT:  dynamicLookup interface  '' bar true
  // DUPLICATE:  dynamicLookup interface  '' bar true
  // DUPLICATE-NOT:  dynamicLookup interface  '' bar true
  func bar(_ x: Int, y: Int) {}
  func bar(_ str: String) {}
    
  // CHECK-DAG:  dynamicLookup implementation  '' prop true
  // CHECK-DAG:  dynamicLookup interface  '' prop true
  var prop: String?

  // CHECK-DAG:  dynamicLookup implementation  '' unusedProp true
  // CHECK-DAG:  dynamicLookup interface  '' unusedProp true
  var unusedProp: Int = 0
  

  // CHECK-DAG:  dynamicLookup implementation  '' classFunc true
  // CHECK-DAG:  dynamicLookup interface  '' classFunc true
  class func classFunc() {}
}

func getAnyObject() -> AnyObject? { return nil }

func testDynamicLookup(_ obj: AnyObject) {
  // CHECK-DAG:  dynamicLookup interface  '' description false
  _ = obj.description
  // CHECK-DAG:  dynamicLookup interface  '' method false
  _ = obj.method(5, with: 5.0 as Double)
  
  // TODO: unable to resolve ambiguity
  // C/HECK-DAG:  dynamicLookup interface  '' subscript false
  // _ = obj[2] as Any
  // _ = obj[2] as Any!
}

// CHECK-DAG:  dynamicLookup interface  '' counter false
let globalUse = getAnyObject()?.counter

// NEGATIVE-NOT:  dynamicLookup interface  '' cat1Method false
// NEGATIVE-NOT:  dynamicLookup interface  '' unusedProp false

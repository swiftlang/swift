// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module -emit-module-path %t/Intermediate.swiftmodule -parse-as-library %t/Intermediate.swift -enable-library-evolution
//
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %t/Client.swift -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

//--- Intermediate.swift

import Foundation

public func hasDefaultArgument(_ n: NSNotification = NSNotification()) { }

@_alwaysEmitIntoClient
public func aeicFuncUsingWeakVar() {
  _ = weak_variable
}

@_alwaysEmitIntoClient
public func aeicFuncUsingStrongVar() {
  _ = strong_variable
}

@_alwaysEmitIntoClient
public func aeicFuncCallingAlwaysAvailableFunc() {
  always_available_function()
}


//--- Client.swift

import Intermediate
@_weakLinked import Foundation

// Symbols from `Foundation` should have weak linkage even when the references
// to them are inlined from `Intermediate`, which imported `Foundation` without
// `@_weakLinked`.

func testDefaultArguments() {
  // CHECK-DAG: @"OBJC_CLASS_$_NSNotification" = extern_weak global %objc_class
  hasDefaultArgument()
}

func testAlwaysEmitIntoClient() {
  // CHECK-DAG: @weak_variable = extern_weak global
  aeicFuncUsingWeakVar()
  
  // CHECK-DAG: @strong_variable = extern_weak global
  aeicFuncUsingStrongVar()
  
  // CHECK-DAG: declare extern_weak void @always_available_function()
  aeicFuncCallingAlwaysAvailableFunc()
}

// RUN: %empty-directory(%t)

// Ensure @_backDeploy attributes and function bodies are printed in
// swiftinterface files.
// RUN: %target-swiftc_driver -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -enable-library-evolution -verify-emitted-module-interface -module-name Test %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix CHECK < %t/Test.swiftinterface

// FIXME(backDeploy): Remove this step in favor of a test that exercises using
// a back deployed API from a test library so that we can avoid -merge-modules

// Ensure @_backDeploy attributes and function bodies are present after
// deserializing .swiftmodule files.
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test
// RUN: %FileCheck %s --check-prefix FROMMODULE --check-prefix CHECK < %t/TestFromModule.swiftinterface

public struct TopLevelStruct {
  // CHECK: @_backDeploy(macOS 12.0)
  // FROMSOURCE: public func backDeployedFunc_SinglePlatform() -> Swift.Int { return 42 }
  // FROMMODULE: public func backDeployedFunc_SinglePlatform() -> Swift.Int
  @available(macOS 11.0, *)
  @_backDeploy(macOS 12.0)
  public func backDeployedFunc_SinglePlatform() -> Int { return 42 }
  
  // CHECK: @_backDeploy(macOS 12.0)
  // CHECK: @_backDeploy(iOS 15.0)
  // FROMSOURCE: public func backDeployedFunc_MultiPlatform() -> Swift.Int { return 43 }
  // FROMMODULE: public func backDeployedFunc_MultiPlatform() -> Swift.Int
  @available(macOS 11.0, iOS 14.0, *)
  @_backDeploy(macOS 12.0, iOS 15.0)
  public func backDeployedFunc_MultiPlatform() -> Int { return 43 }

  // CHECK: @_backDeploy(macOS 12.0)
  // FROMSOURCE: public var backDeployedComputedProperty: Swift.Int {
  // FROMSOURCE:   get { 44 }
  // FROMSOURCE: }
  // FROMMODULE: public var backDeployedComputedProperty: Swift.Int
  @available(macOS 11.0, *)
  @_backDeploy(macOS 12.0)
  public var backDeployedComputedProperty: Int { 44 }

  // CHECK: @_backDeploy(macOS 12.0)
  // FROMSOURCE: public var backDeployedPropertyWithAccessors: Swift.Int {
  // FROMSOURCE:   get { 45 }
  // FROMSOURCE:   set(newValue) { print("set property") }
  // FROMSOURCE: }
  // FROMMODULE: public var backDeployedPropertyWithAccessors: Swift.Int
  @available(macOS 11.0, *)
  @_backDeploy(macOS 12.0)
  public var backDeployedPropertyWithAccessors: Int {
    get { 45 }
    set(newValue) { print("set property") }
  }

  // CHECK: @_backDeploy(macOS 12.0)
  // FROMSOURCE: public subscript(index: Swift.Int) -> Swift.Int {
  // FROMSOURCE:   get { 46 }
  // FROMSOURCE:   set(newValue) { print("set subscript") }
  // FROMSOURCE: }
  // FROMMODULE: public subscript(index: Swift.Int) -> Swift.Int
  @available(macOS 11.0, *)
  @_backDeploy(macOS 12.0)
  public subscript(index: Int) -> Int {
    get { 46 }
    set(newValue) { print("set subscript") }
  }
}

// CHECK: @_backDeploy(macOS 12.0)
// FROMSOURCE: public func backDeployTopLevelFunc() -> Swift.Int { return 47 }
// FROMMODULE: public func backDeployTopLevelFunc() -> Swift.Int
@available(macOS 11.0, *)
@_backDeploy(macOS 12.0)
public func backDeployTopLevelFunc() -> Int { return 47 }

// FIXME(backDeploy): Availability macros should be supported

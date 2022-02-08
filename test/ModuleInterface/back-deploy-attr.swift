// RUN: %empty-directory(%t)

// Ensure @_backDeploy attributes and function bodies are printed in
// swiftinterface files.
// RUN: %swiftc_driver -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -enable-library-evolution -verify-emitted-module-interface -module-name Test %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix CHECK < %t/Test.swiftinterface

// FIXME(backDeploy): Remove this step in favor of a test that exercises using
// a back deployed API from a test library so that we can avoid -merge-modules

// Ensure @_backDeploy attributes and function bodies are present after
// deserializing .swiftmodule files.
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test
// RUN: %FileCheck %s --check-prefix FROMMODULE --check-prefix CHECK < %t/TestFromModule.swiftinterface

// FIXME(backDeploy): Verify that function bodies are emitted

public struct TopLevelStruct {
  // CHECK: @_backDeploy(macOS 11.0)
  // CHECK: public func backDeployedFunc1_SinglePlatform() -> Swift.Int
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0)
  public func backDeployedFunc1_SinglePlatform() -> Int {
    return 42
  }
  
  // CHECK: @_backDeploy(macOS 11.0)
  // CHECK: @_backDeploy(iOS 14.0)
  // CHECK: public func backDeployedFunc2_MultiPlatform() -> Swift.Int
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0, iOS 14.0)
  public func backDeployedFunc2_MultiPlatform() -> Int {
    return 43
  }
}

// CHECK: @_backDeploy(macOS 11.0)
// CHECK: public func backDeployTopLevelFunc() -> Swift.Int
@available(macOS 12.0, *)
@_backDeploy(macOS 11.0)
public func backDeployTopLevelFunc() -> Int {
  return 42
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s \
// RUN:   -define-availability "_macOS12_1:macOS 12.1" \
// RUN:   -define-availability "_myProject 1.0:macOS 12.1, iOS 15.1"
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface)
// RUN: %FileCheck %s < %t/Test.swiftinterface

public struct TopLevelStruct {
  // CHECK: @_backDeploy(before: macOS 12.0)
  // CHECK: public func backDeployedFunc_SinglePlatform() -> Swift.Int { return 42 }
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedFunc_SinglePlatform() -> Int { return 42 }
  
  // CHECK: @_backDeploy(before: macOS 12.0)
  // CHECK: @_backDeploy(before: iOS 15.0)
  // CHECK: public func backDeployedFunc_MultiPlatform() -> Swift.Int { return 43 }
  @available(macOS 11.0, iOS 14.0, *)
  @_backDeploy(before: macOS 12.0, iOS 15.0)
  public func backDeployedFunc_MultiPlatform() -> Int { return 43 }

  // CHECK: @_backDeploy(before: macOS 12.0)
  // CHECK: public var backDeployedComputedProperty: Swift.Int {
  // CHECK:   get { 44 }
  // CHECK: }
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public var backDeployedComputedProperty: Int { 44 }

  // CHECK: @_backDeploy(before: macOS 12.0)
  // CHECK: public var backDeployedPropertyWithAccessors: Swift.Int {
  // CHECK:   get { 45 }
  // CHECK: }
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public var backDeployedPropertyWithAccessors: Int {
    get { 45 }
  }

  // CHECK: @_backDeploy(before: macOS 12.0)
  // CHECK: public subscript(index: Swift.Int) -> Swift.Int {
  // CHECK:   get { 46 }
  // CHECK: }
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public subscript(index: Int) -> Int {
    get { 46 }
  }
}

// CHECK: @_backDeploy(before: macOS 12.0)
// CHECK: public func backDeployTopLevelFunc1() -> Swift.Int { return 47 }
@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
public func backDeployTopLevelFunc1() -> Int { return 47 }

// MARK: - Availability macros

// CHECK: @_backDeploy(before: macOS 12.1)
// CHECK: public func backDeployTopLevelFunc2() -> Swift.Int { return 48 }
@available(macOS 11.0, *)
@_backDeploy(before: _macOS12_1)
public func backDeployTopLevelFunc2() -> Int { return 48 }

// CHECK: @_backDeploy(before: macOS 12.1)
// CHECK: @_backDeploy(before: iOS 15.1)
// CHECK: public func backDeployTopLevelFunc3() -> Swift.Int { return 49 }
@available(macOS 11.0, iOS 14.0, *)
@_backDeploy(before: _myProject 1.0)
public func backDeployTopLevelFunc3() -> Int { return 49 }

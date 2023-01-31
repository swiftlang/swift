// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: OS=macosx

@_backDeploy(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
public func topLevelFunc() {}

public struct TopLevelStruct {
  @_backDeploy(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
  public func methodInStruct() {}
}

extension TopLevelStruct {
  @_backDeploy(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
  public func methodInExtension() {}
}

@available(macOS 11, *)
@_backDeploy(before: macOS 12) // Ok, introduced availability is earlier than macOS 12
public func availableBeforeBackDeployment() {}

@available(macOS 12, *) // expected-note {{'availableSameVersionAsBackDeployment()' was introduced in macOS 12}}
@_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'availableSameVersionAsBackDeployment()' is not available before macOS 12}}
public func availableSameVersionAsBackDeployment() {}

@available(macOS 12.1, *) // expected-note {{'availableAfterBackDeployment()' was introduced in macOS 12.1}}
@_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'availableAfterBackDeployment()' is not available before macOS 12}}
public func availableAfterBackDeployment() {}

@available(macOS 12, iOS 13, *)
@_backDeploy(before: iOS 12) // This is invalid but it can only be diagnosed when building for iOS
public func availableAfterBackDeploymentForInactiveAttribute() {}

@available(macOS 12, *) // expected-note {{'memberFunc()' was introduced in macOS 12}}
public struct AvailableMacOS12Struct {
  @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'memberFunc()' is not available before macOS 12}}
  public func memberFunc() {}
}

@available(macOS 12, *) // expected-note {{'methodInExtensionAvailableMacOS12()' was introduced in macOS 12}}
extension TopLevelStruct {
  @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'methodInExtensionAvailableMacOS12()' is not available before macOS 12}}
  public func methodInExtensionAvailableMacOS12() {}
}

extension TopLevelStruct {
  @available(macOS 12, *) // expected-note {{getter for 'propertyAvailableMacOS12' was introduced in macOS 12}}
  public var propertyAvailableMacOS12: Int {
    @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because getter for 'propertyAvailableMacOS12' is not available before macOS 12}}
    get { 0 }
  }
}

@available(macOS, unavailable) // expected-note {{'unavailableMacOSFunc()' has been explicitly marked unavailable here}}
@_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'unavailableMacOSFunc()' is unavailable on macOS}}
public func unavailableMacOSFunc() {}

@available(macOS, unavailable)
@available(iOS, unavailable)
@_backDeploy(before: iOS 12) // This is invalid but it can only be diagnosed when building for iOS
public func unavailableForInactiveAttributeFunc() {}

@available(*, unavailable) // expected-note {{'alwaysUnavailableFunc()' has been explicitly marked unavailable here}}
@_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'alwaysUnavailableFunc()' is unavailable on macOS}}
public func alwaysUnavailableFunc() {}

@available(macOS, unavailable) // expected-note {{'memberFunc()' has been explicitly marked unavailable here}}
public struct UnavailableMacOSStruct {
  @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'memberFunc()' is unavailable on macOS}}
  public func memberFunc() {}
}

@available(macOS, unavailable) // expected-note {{'methodInUnavailableExtension()' has been explicitly marked unavailable here}}
extension TopLevelStruct {
  @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because 'methodInUnavailableExtension()' is unavailable on macOS}}
  public func methodInUnavailableExtension() {}
}

extension TopLevelStruct {
  @available(macOS, unavailable) // expected-note {{getter for 'unavailableMacOSProperty' has been explicitly marked unavailable here}}
  public var unavailableMacOSProperty: Int {
    @_backDeploy(before: macOS 12) // expected-error {{'@_backDeploy' has no effect because getter for 'unavailableMacOSProperty' is unavailable on macOS}}
    get { 0 }
  }
}

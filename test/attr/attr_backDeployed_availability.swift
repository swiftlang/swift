// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: OS=macosx

@backDeployed(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
public func topLevelFunc() {}

public struct TopLevelStruct {
  @backDeployed(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
  public func methodInStruct() {}
}

extension TopLevelStruct {
  @backDeployed(before: macOS 12) // Ok, introduced availability is inferred to be macOS 10.9
  public func methodInExtension() {}
}

@available(macOS 11, *)
@backDeployed(before: macOS 12) // Ok, introduced availability is earlier than macOS 12
public func availableBeforeBackDeployment() {}

@available(macOS 12, *) // expected-note {{'availableSameVersionAsBackDeployment()' was introduced in macOS 12}}
@backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'availableSameVersionAsBackDeployment()' is not available before macOS 12}}
public func availableSameVersionAsBackDeployment() {}

@available(macOS 12.1, *) // expected-note {{'availableAfterBackDeployment()' was introduced in macOS 12.1}}
@backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'availableAfterBackDeployment()' is not available before macOS 12}}
public func availableAfterBackDeployment() {}

@available(macOS 12, iOS 13, *)
@backDeployed(before: iOS 12) // This is invalid but it can only be diagnosed when building for iOS
public func availableAfterBackDeploymentForInactiveAttribute() {}

@available(macOS 12, *) // expected-note {{'memberFunc()' was introduced in macOS 12}}
public struct AvailableMacOS12Struct {
  @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'memberFunc()' is not available before macOS 12}}
  public func memberFunc() {}
}

@available(macOS 12, *) // expected-note {{'methodInExtensionAvailableMacOS12()' was introduced in macOS 12}}
extension TopLevelStruct {
  @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'methodInExtensionAvailableMacOS12()' is not available before macOS 12}}
  public func methodInExtensionAvailableMacOS12() {}
}

extension TopLevelStruct {
  @available(macOS 12, *) // expected-note {{getter for 'propertyAvailableMacOS12' was introduced in macOS 12}}
  public var propertyAvailableMacOS12: Int {
    @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because getter for 'propertyAvailableMacOS12' is not available before macOS 12}}
    get { 0 }
  }
}

@available(macOS, unavailable) // expected-note {{'unavailableMacOSFunc()' has been explicitly marked unavailable here}}
@backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'unavailableMacOSFunc()' is unavailable on macOS}}
public func unavailableMacOSFunc() {}

@available(macOS, unavailable)
@available(iOS, unavailable)
@backDeployed(before: iOS 12) // This is invalid but it can only be diagnosed when building for iOS
public func unavailableForInactiveAttributeFunc() {}

@available(*, unavailable) // expected-note {{'alwaysUnavailableFunc()' has been explicitly marked unavailable here}}
@backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'alwaysUnavailableFunc()' is unavailable on macOS}}
public func alwaysUnavailableFunc() {}

@available(macOS, unavailable) // expected-note {{'memberFunc()' has been explicitly marked unavailable here}}
public struct UnavailableMacOSStruct {
  @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'memberFunc()' is unavailable on macOS}}
  public func memberFunc() {}
}

@available(macOS, unavailable) // expected-note {{'methodInUnavailableExtension()' has been explicitly marked unavailable here}}
extension TopLevelStruct {
  @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because 'methodInUnavailableExtension()' is unavailable on macOS}}
  public func methodInUnavailableExtension() {}
}

extension TopLevelStruct {
  @available(macOS, unavailable) // expected-note {{getter for 'unavailableMacOSProperty' has been explicitly marked unavailable here}}
  public var unavailableMacOSProperty: Int {
    @backDeployed(before: macOS 12) // expected-error {{'@backDeployed' has no effect because getter for 'unavailableMacOSProperty' is unavailable on macOS}}
    get { 0 }
  }
}

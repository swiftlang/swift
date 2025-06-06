// RUN: %empty-directory(%t/mock-sdk)
// RUN: cp %S/../Inputs/MockPlatformRemapSDKConfig/SDKSettings.json %t/mock-sdk/SDKSettings.json
// RUN: %swift -typecheck -verify -parse-stdlib -target arm64-apple-xros1.0 %s -sdk %t/mock-sdk

@backDeployed(before: visionOS 2) // Ok, introduced availability is inferred to be visionOS epoch
public func topLevelFunc() {}

public struct TopLevelStruct {
  @backDeployed(before: visionOS 2) // Ok, introduced availability is inferred to be visionOS epoch
  public func methodInStruct() {}
}

extension TopLevelStruct {
  @backDeployed(before: visionOS 2) // Ok, introduced availability is inferred to be visionOS epoch
  public func methodInExtension() {}
}

@available(visionOS 1.1, *)
@backDeployed(before: visionOS 2) // Ok, introduced availability is earlier than visionOS 2
public func availableBeforeBackDeployment() {}

@available(iOS 17, *)
@backDeployed(before: iOS 17.4) // Ok, introduced availability is earlier than visionOS 1.1
public func availableOniOSBeforeBackDeploymentOniOS() {}

@available(iOS 15, *)
@backDeployed(before: iOS 16) // Ok, both iOS availability and back deployment are earlier than the visionOS epoch
public func availableOnEarlyiOSBeforeBackDeploymentOnEarlyiOS() {}

@available(iOS 13, *)
@backDeployed(before: visionOS 2) // Ok, re-mapped introduced availability is earlier than visionOS 2
public func availableOniOSBeforeBackDeployment() {}

@available(iOS 17.4, visionOS 1, *)
@backDeployed(before: visionOS 1.1) // Ok, introduced availability is earlier than visionOS 1.1
public func availableBeforeBackDeploymentOnVisionOSNotOniOS() {}

@available(visionOS 2, *) // expected-note {{'availableSameVersionAsBackDeployment()' was introduced in visionOS 2}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'availableSameVersionAsBackDeployment()' is not available before visionOS 2}}
public func availableSameVersionAsBackDeployment() {}

@available(iOS 17.4, *) // expected-note {{'availableSameRemappedVersionAsBackDeployment()' was introduced in visionOS 1.1}}
@backDeployed(before: visionOS 1.1) // expected-error {{'@backDeployed' has no effect because 'availableSameRemappedVersionAsBackDeployment()' is not available before visionOS 1.1}}
public func availableSameRemappedVersionAsBackDeployment() {}

@available(iOS 17, visionOS 2, *) // expected-note {{'availableSameVersionAsBackDeploymentAndAlsoAvailableEarlierOniOS()' was introduced in visionOS 2}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'availableSameVersionAsBackDeploymentAndAlsoAvailableEarlierOniOS()' is not available before visionOS 2}}
public func availableSameVersionAsBackDeploymentAndAlsoAvailableEarlierOniOS() {}

@available(visionOS 2.1, *) // expected-note {{'availableAfterBackDeployment()' was introduced in visionOS 2.1}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'availableAfterBackDeployment()' is not available before visionOS 2}}
public func availableAfterBackDeployment() {}

@available(iOS 99, *) // expected-note {{'availableOniOSAfterBackDeploymentOniOS()' was introduced in iOS 99}}
@backDeployed(before: iOS 17.4) // expected-error {{'@backDeployed' has no effect because 'availableOniOSAfterBackDeploymentOniOS()' is not available before visionOS 1.1}}
public func availableOniOSAfterBackDeploymentOniOS() {}

@available(visionOS 2, *) // expected-note {{'memberFuncBackDeploymentSame()' was introduced in visionOS 2}}
public struct AvailableVisionOSStruct {
  @backDeployed(before: visionOS 2.1)
  public func memberFunc() {}

  @backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'memberFuncBackDeploymentSame()' is not available before visionOS 2}}
  public func memberFuncBackDeploymentSame() {}
}

@available(iOS 17.4, *) // expected-note {{'memberFuncBackDeploymentSame()' was introduced in visionOS 1.1}}
public struct AvailableiOSStruct {
  @backDeployed(before: visionOS 2)
  public func memberFunc() {}

  @backDeployed(before: visionOS 1.1) // expected-error {{'@backDeployed' has no effect because 'memberFuncBackDeploymentSame()' is not available before visionOS 1.1}}
  public func memberFuncBackDeploymentSame() {}
}

@available(*, unavailable) // expected-note {{'alwaysUnavailableFunc()' has been explicitly marked unavailable here}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'alwaysUnavailableFunc()' is unavailable on visionOS}}
public func alwaysUnavailableFunc() {}

@available(visionOS, unavailable) // expected-note {{'unavailableOnVisionOSFunc()' has been explicitly marked unavailable here}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'unavailableOnVisionOSFunc()' is unavailable on visionOS}}
public func unavailableOnVisionOSFunc() {}

@available(visionOSApplicationExtension, unavailable)
@backDeployed(before: visionOS 2)
public func unavailableForVisionOSExtensionsFunc() {}

@available(iOS, unavailable) // expected-note {{'unavailableOniOSFunc()' has been explicitly marked unavailable here}}
@backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'unavailableOniOSFunc()' is unavailable on visionOS}}
public func unavailableOniOSFunc() {}

@available(iOSApplicationExtension, unavailable)
@backDeployed(before: visionOS 2)
public func unavailableForiOSExtensionsFunc() {}

@available(visionOS, unavailable)
@backDeployed(before: iOS 17.4)
public func unavailableOnVisionOSBackDeployedOniOSFunc() {}

@available(visionOS, unavailable) // expected-note {{'memberFunc()' has been explicitly marked unavailable here}}
public struct UnavailableVisionOSStruct {
  @backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'memberFunc()' is unavailable on visionOS}}
  public func memberFunc() {}
}

@available(iOS, unavailable) // expected-note {{'memberFunc()' has been explicitly marked unavailable here}}
public struct UnavailableiOSStruct {
  @backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'memberFunc()' is unavailable on visionOS}}
  public func memberFunc() {}
}

@available(visionOS, unavailable) // expected-note {{'methodInUnavailableExtension()' has been explicitly marked unavailable here}}
extension TopLevelStruct {
  @backDeployed(before: visionOS 2) // expected-error {{'@backDeployed' has no effect because 'methodInUnavailableExtension()' is unavailable on visionOS}}
  public func methodInUnavailableExtension() {}
}

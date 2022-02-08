// RUN: %target-typecheck-verify-swift -parse-as-library

// MARK: - Valid declarations

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0)
public func backDeployedTopLevelFunc() {}

// FIXME(backDeploy): Availability macros should be supported

public class TopLevelClass {
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0)
  public func backDeployedMemberFunc() {}

  // FIXME(backDeploy): Computed properties should be supported
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
  public var backDeployedComputedProperty: Int { 98 }

  // FIXME(backDeploy): Subscripts should be supported
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
  subscript(index: Int) -> Int {
    get { return 1 }
    set(newValue) {}
  }
}

// MARK: - Unsupported declaration types

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public class CannotBackDeployClass {
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
  public var cannotBackDeploystoredProperty: Int = 83
}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public struct CannotBackDeployStruct {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public enum CannotBackDeployEnum {
  @available(macOS 12.0, *)
  @_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
  case cannotBackDeployEnumCase
}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public var cannotBackDeployTopLevelVar = 79

// MARK: - Incompatible declarations

// FIXME(backDeploy): Test explicit @available is required
// FIXME(backDeploy): Test @available is compatible
// FIXME(backDeploy): Test public is required
// FIXME(backDeploy): Test @inlinable, @_alwaysEmitIntoClient, @transparent are rejected

// MARK: - Attribute parsing

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, unknownOS 1.0) // expected-warning {{unknown platform 'unknownOS' for attribute '@_backDeploy'}}
public func unknownOSFunc() {}

@available(macOS 12.0, *)
@_backDeploy(@) // expected-error {{expected platform in '@_backDeploy' attribute}}
public func badPlatformFunc1() {}

@available(macOS 12.0, *)
@_backDeploy(@ 12.0) // expected-error {{expected platform in '@_backDeploy' attribute}}
public func badPlatformFunc2() {}

@available(macOS 12.0, *)
@_backDeploy(macOS) // expected-error {{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc1() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, iOS) // expected-error {{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc2() {}

@available(macOS 12.0, *)
@_backDeploy(macOS, iOS) // expected-error 2{{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc3() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, iOS 14.0,) // expected-error {{unexpected ',' separator}}
public func unexpectedSeparatorFunc() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0.1) // expected-warning {{'@_backDeploy' only uses major and minor version number}}
public func patchVersionFunc() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, * 9.0) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func wildcardWithVersionFunc() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, *) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func trailingWildcardFunc() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, *, iOS 14.0) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func embeddedWildcardFunc() {}

// FIXME(backDeploy): Expect error for duplicate platforms in same attribute
@available(macOS 12.0, *)
@_backDeploy(macOS 11.0, macOS 10.0)
public func duplicatePlatformsFunc1() {}

// FIXME(backDeploy): Expect error for duplicate platforms accross multiple attributes
@available(macOS 12.0, *)
@_backDeploy(macOS 11.0)
@_backDeploy(macOS 10.0)
public func duplicatePlatformsFunc2() {}

@available(macOS 12.0, *)
@_backDeploy() // expected-error {{expected at least one platform version in in '@_backDeploy' attribute}}
public func zeroPlatformVersionsFunc() {}

@available(macOS 12.0, *)
@_backDeploy // expected-error {{expected '(' in '_backDeploy' attribute}}
public func expectedLeftParenFunc() {}

@available(macOS 12.0, *)
@_backDeploy(macOS 11.0 // expected-note {{to match this opening '('}}
public func expectedRightParenFunc() {} // expected-error {{expected ')' in '@_backDeploy' argument list}}

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -define-availability "_myProject 2.0:macOS 12.0"

// MARK: - Valid declarations

// OK: top level functions
@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
public func backDeployedTopLevelFunc() {}

// OK: internal decls may be back deployed when @usableFromInline
@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
@usableFromInline
internal func backDeployedUsableFromInlineTopLevelFunc() {}

// OK: function/property/subscript decls in a struct
public struct TopLevelStruct {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedMethod() {}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public var backDeployedComputedProperty: Int { 98 }

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public subscript(_ index: Int) -> Int { index }

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public var readWriteProperty: Int {
    get { 42 }
    set(newValue) {}
  }

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public subscript(at index: Int) -> Int {
    get { 42 }
    set(newValue) {}
  }

  public var explicitReadAndModify: Int {
    @available(macOS 11.0, *)
    @_backDeploy(before: macOS 12.0)
    _read { yield 42 }

    @available(macOS 11.0, *)
    @_backDeploy(before: macOS 12.0)
    _modify {}
  }
}

// OK: final function decls in a non-final class
public class TopLevelClass {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  final public func backDeployedFinalMethod() {}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  final public var backDeployedFinalComputedProperty: Int { 98 }

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public static func backDeployedStaticMethod() {}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public final class func backDeployedClassMethod() {}
}

// OK: function decls in a final class
final public class FinalTopLevelClass {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedMethod() {}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public var backDeployedComputedProperty: Int { 98 }
}

// OK: final function decls on an actor
@available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
public actor TopLevelActor {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  final public func finalActorMethod() {}

  // OK: actor methods are effectively final
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func actorMethod() {}
}

// OK: function decls in extension on public types
extension TopLevelStruct {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedExtensionMethod() {}
}

extension TopLevelClass {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  final public func backDeployedExtensionMethod() {}
}

extension FinalTopLevelClass {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedExtensionMethod() {}
}

public protocol TopLevelProtocol {}

extension TopLevelProtocol {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedExtensionMethod() {}
}

// MARK: - Unsupported declaration types

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public class CannotBackDeployClass {}

public final class CannotBackDeployClassInitDeinit {
  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to initializer declarations}}
  public init() {}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to deinitializer declarations}}
  deinit {}
}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public struct CannotBackDeployStruct {
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' must not be used on stored properties}}
  public var cannotBackDeployStoredProperty: Int = 83

  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' must not be used on stored properties}}
  public lazy var cannotBackDeployLazyStoredProperty: Int = 15
}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public enum CannotBackDeployEnum {
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
  case cannotBackDeployEnumCase
}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' must not be used on stored properties}}
public var cannotBackDeployTopLevelVar = 79

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
extension TopLevelStruct {}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
protocol CannotBackDeployProtocol {}

@available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' attribute cannot be applied to this declaration}}
public actor CannotBackDeployActor {}

// MARK: - Function body diagnostics

public struct FunctionBodyDiagnostics {
  public func publicFunc() {}
  @usableFromInline func usableFromInlineFunc() {}
  func internalFunc() {} // expected-note {{instance method 'internalFunc()' is not '@usableFromInline' or public}}
  fileprivate func fileprivateFunc() {} // expected-note {{instance method 'fileprivateFunc()' is not '@usableFromInline' or public}}
  private func privateFunc() {} // expected-note {{instance method 'privateFunc()' is not '@usableFromInline' or public}}

  @available(macOS 11.0, *)
  @_backDeploy(before: macOS 12.0)
  public func backDeployedMethod() {
    struct Nested {} // expected-error {{type 'Nested' cannot be nested inside a '@_backDeploy' function}}

    publicFunc()
    usableFromInlineFunc()
    internalFunc() // expected-error {{instance method 'internalFunc()' is internal and cannot be referenced from a '@_backDeploy' function}}
    fileprivateFunc() // expected-error {{instance method 'fileprivateFunc()' is fileprivate and cannot be referenced from a '@_backDeploy' function}}
    privateFunc() // expected-error {{instance method 'privateFunc()' is private and cannot be referenced from a '@_backDeploy' function}}
  }
}

// MARK: - Incompatible declarations

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' may not be used on fileprivate declarations}}
fileprivate func filePrivateFunc() {}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' may not be used on private declarations}}
private func privateFunc() {}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' may not be used on internal declarations}}
internal func internalFunc() {}

private struct PrivateTopLevelStruct {
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' may not be used on private declarations}}
  public func effectivelyPrivateFunc() {}
}

public class TopLevelClass2 {
  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' cannot be applied to a non-final instance method}}
  public func nonFinalMethod() {}

  @_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' cannot be applied to a non-final class method}}
  public class func nonFinalClassMethod() {}
}

@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' requires that 'missingAllAvailabilityFunc()' have explicit availability for macOS}}
public func missingAllAvailabilityFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0, iOS 15.0) // expected-error {{'@_backDeploy' requires that 'missingiOSAvailabilityFunc()' have explicit availability for iOS}}
public func missingiOSAvailabilityFunc() {}

@available(macOS 12.0, *)
@_backDeploy(before: macOS 12.0) // expected-error {{'@_backDeploy' has no effect because 'availableSameVersionAsBackDeployment()' is not available before macOS 12.0}}
public func availableSameVersionAsBackDeployment() {}

@available(macOS 12.1, *)
@_backDeploy(before: macOS 12.0) // expected-error {{'availableAfterBackDeployment()' is not available before macOS 12.0}}
public func availableAfterBackDeployment() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0, macOS 13.0) // expected-error {{'@_backDeploy' contains multiple versions for macOS}}
public func duplicatePlatformsFunc1() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
@_backDeploy(before: macOS 13.0) // expected-error {{'@_backDeploy' contains multiple versions for macOS}}
public func duplicatePlatformsFunc2() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
@_alwaysEmitIntoClient // expected-error {{'@_alwaysEmitIntoClient' cannot be applied to a back deployed global function}}
public func alwaysEmitIntoClientFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
@inlinable // expected-error {{'@inlinable' cannot be applied to a back deployed global function}}
public func inlinableFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0)
@_transparent // expected-error {{'@_transparent' cannot be applied to a back deployed global function}}
public func transparentFunc() {}

// MARK: - Attribute parsing

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0, unknownOS 1.0) // expected-warning {{unknown platform 'unknownOS' for attribute '@_backDeploy'}}
public func unknownOSFunc() {}

@_backDeploy(before: @) // expected-error {{expected platform in '@_backDeploy' attribute}}
public func badPlatformFunc1() {}

@_backDeploy(before: @ 12.0) // expected-error {{expected platform in '@_backDeploy' attribute}}
public func badPlatformFunc2() {}

@_backDeploy(before: macOS) // expected-error {{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc1() {}

@_backDeploy(before: macOS 12.0, iOS) // expected-error {{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc2() {}

@_backDeploy(before: macOS, iOS) // expected-error 2{{expected version number in '@_backDeploy' attribute}}
public func missingVersionFunc3() {}

@available(macOS 11.0, iOS 14.0, *)
@_backDeploy(before: macOS 12.0, iOS 15.0,) // expected-error {{unexpected ',' separator}}
public func unexpectedSeparatorFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0.1) // expected-warning {{'@_backDeploy' only uses major and minor version number}}
public func patchVersionFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0, * 9.0) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func wildcardWithVersionFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0, *) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func trailingWildcardFunc() {}

@available(macOS 11.0, iOS 14.0, *)
@_backDeploy(before: macOS 12.0, *, iOS 15.0) // expected-warning {{* as platform name has no effect in '@_backDeploy' attribute}}
public func embeddedWildcardFunc() {}

@_backDeploy(before: _myProject 3.0) // expected-error {{reference to undefined version '3.0' for availability macro '_myProject'}}
public func macroVersioned() {}

@_backDeploy(before: _myProject) // expected-error {{reference to undefined version '0' for availability macro '_myProject'}}
public func missingMacroVersion() {}

// Fall back to the default diagnostic when the macro is unknown.
@_backDeploy(before: _unknownMacro) // expected-warning {{unknown platform '_unknownMacro' for attribute '@_backDeploy'}}
// expected-error@-1 {{expected version number in '@_backDeploy' attribute}}
public func unknownMacroMissingVersion() {}

@_backDeploy(before: _unknownMacro 1.0) // expected-warning {{unknown platform '_unknownMacro' for attribute '@_backDeploy'}}
// expected-error@-1 {{expected at least one platform version in '@_backDeploy' attribute}}
public func unknownMacroVersioned() {}

@available(macOS 11.0, *)
@_backDeploy(before: _unknownMacro 1.0, _myProject 2.0) // expected-warning {{unknown platform '_unknownMacro' for attribute '@_backDeploy'}}
public func knownAndUnknownMacroVersioned() {}

@_backDeploy() // expected-error {{expected 'before:' in '@_backDeploy' attribute}}
// expected-error@-1 {{expected at least one platform version in '@_backDeploy' attribute}}
public func emptyAttributeFunc() {}

@available(macOS 11.0, *)
@_backDeploy(macOS 12.0) // expected-error {{expected 'before:' in '@_backDeploy' attribute}} {{14-14=before:}}
public func missingBeforeFunc() {}

@_backDeploy(before) // expected-error {{expected ':' after 'before' in '@_backDeploy' attribute}} {{20-20=:}}
// expected-error@-1 {{expected at least one platform version in '@_backDeploy' attribute}}
public func missingColonAfterBeforeFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before macOS 12.0) // expected-error {{expected ':' after 'before' in '@_backDeploy' attribute}} {{20-20=:}}
public func missingColonBetweenBeforeAndPlatformFunc() {}

@available(macOS 11.0, *)
@_backDeploy(before: macOS 12.0,) // expected-error {{unexpected ',' separator}} {{32-33=}}
public func unexpectedTrailingCommaFunc() {}

@available(macOS 11.0, iOS 14.0, *)
@_backDeploy(before: macOS 12.0,, iOS 15.0) // expected-error {{unexpected ',' separator}} {{33-34=}}
public func extraCommaFunc() {}

@_backDeploy(before:) // expected-error {{expected at least one platform version in '@_backDeploy' attribute}}
public func emptyPlatformVersionsFunc() {}

@_backDeploy // expected-error {{expected '(' in '@_backDeploy' attribute}}
public func expectedLeftParenFunc() {}

@_backDeploy(before: macOS 12.0 // expected-note {{to match this opening '('}}
public func expectedRightParenFunc() {} // expected-error {{expected ')' in '@_backDeploy' argument list}}

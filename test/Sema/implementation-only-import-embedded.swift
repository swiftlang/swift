/// Test @_implementationOnly import exportability diagnostics in embedded mode.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule \
// RUN:   %S/Inputs/implementation-only-imports/indirects.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t\
// RUN:    %S/Inputs/implementation-only-imports/directs.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -define-availability "availMacro:macOS 26.0, iOS 26.0" \
// RUN:   -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_cross_compiling

@_implementationOnly import directs
// expected-warning @-1 {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
import indirects

internal func localInternalFunc() {} // expected-note {{global function 'localInternalFunc()' is not '@usableFromInline' or public}}

@inlinable
public func explicitlyInlinable(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-2 {{struct 'StructFromDirect' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-3 {{struct 'StructFromDirect' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
  _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
  // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}

  if (true) {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
  }

  func nested() {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an '@inlinable' function because 'directs' was imported implementation-only}}
  }
  nested()

  localInternalFunc() // expected-error {{global function 'localInternalFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate() // expected-error {{global function 'implicitlyInlinablePrivate(arg:)' is private and cannot be referenced from an '@inlinable' function}}
  explicitNonInliable()
}

public func implicitlyInlinablePublic(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-2 {{struct 'StructFromDirect' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-3 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}

  _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}

  if (true) {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  }

  func nested() {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  }
  nested()

  localInternalFunc()

  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate()
  explicitNonInliable()

  if #available(availMacro, *) { }
}

private func implicitlyInlinablePrivate(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
// expected-note @-2 {{global function 'implicitlyInlinablePrivate(arg:)' is not '@usableFromInline' or public}}

  _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}

  if (true) {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  }

  func nested() {
    _ = StructFromDirect() // expected-error {{initializer 'init()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
    // expected-error@-1 {{struct 'StructFromDirect' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
  }
  nested()

  localInternalFunc()

  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate()
  explicitNonInliable()
}

@_neverEmitIntoClient
public func explicitNonInliable(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-2 {{struct 'StructFromDirect' cannot be used in a default argument value because 'directs' was imported implementation-only}}
// expected-error @-3 {{cannot use struct 'StructFromDirect' here; 'directs' has been imported as implementation-only}}

  _ = StructFromDirect()

  if (true) {
    _ = StructFromDirect()
  }

  func nested() {
    _ = StructFromDirect()
  }
  nested()

  localInternalFunc()

  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate()
  explicitNonInliable()
}

@_neverEmitIntoClient
internal func explicitNonInliableInternal(arg: StructFromDirect = StructFromDirect()) {
  _ = StructFromDirect()

  if (true) {
    _ = StructFromDirect()
  }

  func nested() {
    _ = StructFromDirect()
  }
  nested()

  localInternalFunc()

  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate()
  explicitNonInliable()

  struct NestedStruct {}
}

struct Accessors {
  public var var1: Int {
    get {
      globalFunctionFromDirect() // expected-error {{global function 'globalFunctionFromDirect()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
      return 0
    }
  }

  @_alwaysEmitIntoClient
  public var var2: Int {
    get {
      globalFunctionFromDirect() // expected-error {{global function 'globalFunctionFromDirect()' cannot be used in an embedded function not marked '@_neverEmitIntoClient' because 'directs' was imported implementation-only}}
      return 0
    }
  }

  @_neverEmitIntoClient
  public var var3: Int {
    get {
      globalFunctionFromDirect()
      return 0
    }
  }

  public var var4: Int {
    @_neverEmitIntoClient
    get {
      globalFunctionFromDirect()
      return 0
    }
  }
}

public func legalAccessToIndirect(arg: StructFromIndirect = StructFromIndirect()) {
  _ = StructFromIndirect()

  if (true) {
    _ = StructFromIndirect()
  }

  func nested() {
    _ = StructFromIndirect()
  }
  nested()
}

public struct ExposedLayoutPublic {
  public var publicField: StructFromDirect // expected-error {{cannot use struct 'StructFromDirect' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; 'directs' has been imported as implementation-only}}

  private var privateField: StructFromDirect // FIXME should error
}

private struct ExposedLayoutPrivate {
  private var privateField: StructFromDirect // FIXME should error
}

/// Test @_implementationOnly internal import exportability diagnostics in embedded mode.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule \
// RUN:   %S/Inputs/implementation-only-imports/indirects.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t \
// RUN:   %S/Inputs/implementation-only-imports/directs.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %s -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_cross_compiling

internal import directs
// expected-note @-1 11 {{struct 'StructFromDirect' imported as 'internal' from 'directs' here}}
// expected-note @-2 6 {{initializer 'init()' imported as 'internal' from 'directs' here}}
import indirects

internal func localInternalFunc() {} // expected-note {{global function 'localInternalFunc()' is not '@usableFromInline' or public}}

typealias AliasToDirect = StructFromDirect

@inlinable
public func explicitlyInlinable(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' is internal and cannot be referenced from a default argument value}}
// expected-error @-2 {{struct 'StructFromDirect' is internal and cannot be referenced from a default argument value}}
// expected-error @-3 {{struct 'StructFromDirect' is internal and cannot be referenced from an '@inlinable' function}}
// expected-error @-4 {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-5 {{struct 'StructFromDirect' is imported by this file as 'internal' from 'directs'}}
  _ = StructFromDirect() // expected-error {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-1 {{struct 'StructFromDirect' is internal and cannot be referenced from an '@inlinable' function}}

  if (true) {
    _ = StructFromDirect() // expected-error {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
    // expected-error@-1 {{struct 'StructFromDirect' is internal and cannot be referenced from an '@inlinable' function}}
  }

  func nested() {
    _ = StructFromDirect() // expected-error {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
    // expected-error@-1 {{struct 'StructFromDirect' is internal and cannot be referenced from an '@inlinable' function}}
  }
  nested()

  localInternalFunc() // expected-error {{global function 'localInternalFunc()' is internal and cannot be referenced from an '@inlinable' function}}

  explicitlyInlinable()
  implicitlyInlinablePublic()
  implicitlyInlinablePrivate() // expected-error {{global function 'implicitlyInlinablePrivate(arg:)' is internal and cannot be referenced from an '@inlinable' function}}
  explicitNonInliable()
}

public func implicitlyInlinablePublic(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' is internal and cannot be referenced from a default argument value}}
// expected-error @-2 {{struct 'StructFromDirect' is internal and cannot be referenced from a default argument value}}
// expected-error @-3 {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-4 {{struct 'StructFromDirect' is imported by this file as 'internal' from 'directs'}}
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

  let _: AliasToDirect
}

internal func implicitlyInlinablePrivate(arg: StructFromDirect = StructFromDirect()) {
// expected-note @-1 {{global function 'implicitlyInlinablePrivate(arg:)' is not '@usableFromInline' or public}}
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

@export(interface)
public func explicitNonInliable(arg: StructFromDirect = StructFromDirect()) {
// expected-error @-1 {{initializer 'init()' is internal and cannot be referenced from a default argument value}}
// expected-error @-2 {{struct 'StructFromDirect' is internal and cannot be referenced from a default argument value}}
// expected-error @-3 {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-4 {{struct 'StructFromDirect' is imported by this file as 'internal' from 'directs'}}
  _ = StructFromDirect()

  if (true) {
    _ = StructFromDirect()
  }

  @export(interface)
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

@export(interface)
internal func explicitNonInliableInternal(arg: StructFromDirect = StructFromDirect()) {
  _ = StructFromDirect()

  if (true) {
    _ = StructFromDirect()
  }

  @export(interface)
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
  public var publicField: StructFromDirect // expected-error {{property cannot be declared public because its type uses an internal type}}
  // expected-note @-1 {{struct 'StructFromDirect' is imported by this file as 'internal' from 'directs'}}

  internal var privateField: StructFromDirect
}

internal struct ExposedLayoutPrivate {
  internal var privateField: StructFromDirect
}

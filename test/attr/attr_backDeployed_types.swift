// RUN: %target-typecheck-verify-swift -parse-as-library -enable-library-evolution

// REQUIRES: OS=macosx

// A back-deployed type is emitted into its clients, so its metadata can't
// require runtime initialization, and it can't have per-image state.

public struct Resilient {
  public var x: Int
}

@frozen public struct Fixed {
  public var x: Int
}

public final class Class {}
public protocol Proto {}

@backDeployed(before: macOS 99)
@frozen public struct Stored {
  public var int: Int
  public var fixed: Fixed
  public var reference: Class
  public var existential: any Proto
  public var function: () -> Int
  public var resilient: Resilient // expected-error {{property 'resilient' of back-deployed struct 'Stored' cannot have type 'Resilient', whose layout is resilient}}
  public var optional: Resilient? // expected-error {{property 'optional' of back-deployed struct 'Stored' cannot have type 'Resilient', whose layout is resilient}}
  public var array: [Resilient] // expected-error {{property 'array' of back-deployed struct 'Stored' cannot have type 'Resilient', whose layout is resilient}}
  public var tuple: (Int, Resilient) // expected-error {{property 'tuple' of back-deployed struct 'Stored' cannot have type 'Resilient', whose layout is resilient}}

  public static var staticStored = 0 // expected-error {{back-deployed struct 'Stored' cannot have static stored property 'staticStored'}}
  public static var staticComputed: Int { 0 }
}

extension Stored {
  public static let extensionStatic = 0 // expected-error {{back-deployed struct 'Stored' cannot have static stored property 'extensionStatic'}}
}

@backDeployed(before: macOS 99)
@frozen public struct Generic<T> {
  public var value: T
  public var resilient: Resilient // expected-error {{property 'resilient' of back-deployed generic struct 'Generic' cannot have type 'Resilient', whose layout is resilient}}
}

@backDeployed(before: macOS 99)
@frozen public enum Payloads {
  case none
  case fixed(Int, Fixed)
  case resilient(Resilient) // expected-error {{enum case 'resilient' of back-deployed enum 'Payloads' cannot have type 'Resilient', whose layout is resilient}}
  indirect case boxed(Resilient)
}

@backDeployed(before: macOS 99)
@frozen public indirect enum AllBoxed {
  case resilient(Resilient)
}

@backDeployed(before: macOS 99)
@frozen public struct Outer {
  @frozen public struct Nested {
    public var x: Int
    public var resilient: Resilient // expected-error {{property 'resilient' of back-deployed struct 'Nested' cannot have type 'Resilient', whose layout is resilient}}
    public static var staticStored = 0 // expected-error {{back-deployed struct 'Nested' cannot have static stored property 'staticStored'}}
  }

  @frozen public enum NestedEnum {
    case a, b
  }

  public struct NotFrozen {} // expected-error {{struct 'NotFrozen' nested in back-deployed struct 'Outer' must be '@frozen'}}
  public final class NestedClass {} // expected-error {{class 'NestedClass' cannot be nested in back-deployed struct 'Outer'}}
  public protocol NestedProto {} // expected-error {{protocol 'NestedProto' cannot be nested in back-deployed struct 'Outer'}}

  // Types that clients can't use don't need to be back-deployed.
  struct InternalNotFrozen {}
  final class InternalClass {}
}

extension Outer {
  @frozen public struct NestedInExtension {
    public var x: Int
  }

  public struct NotFrozenInExtension {} // expected-error {{struct 'NotFrozenInExtension' nested in back-deployed struct 'Outer' must be '@frozen'}}
}

// Declarations that are only available where the type's defining module
// provides it aren't back-deployed along with the type.

func internalHelper() -> Int { 0 } // expected-note 3 {{global function 'internalHelper()' is not '@usableFromInline' or public}}

@backDeployed(before: macOS 99)
@frozen public struct OptOut {
  public var x: Int

  public func backDeployed() -> Int {
    internalHelper() // expected-error {{global function 'internalHelper()' is internal and cannot be referenced from a '@backDeployed' function}}
  }

  @available(macOS 99, *)
  public func newer() -> Int { internalHelper() }

  // Members that are always emitted into clients aren't back-deployed, under
  // either spelling, and are checked as such.
  @export(implementation)
  public func exported() -> Int {
    internalHelper() // expected-error {{global function 'internalHelper()' is internal and cannot be referenced from an '@export(implementation)' function}}
  }

  @_alwaysEmitIntoClient
  public func historical() -> Int {
    internalHelper() // expected-error {{global function 'internalHelper()' is internal and cannot be referenced from an '@export(implementation)' function}}
  }

  @available(macOS 99, *)
  public static var newerStatic = 0

  @available(macOS 99, *)
  public final class NewerClass {}

  @available(macOS 99, *)
  public struct NewerNotFrozen {
    public func member() -> Int { internalHelper() }
  }
}

@available(macOS 99, *)
extension OptOut {
  public func inNewerExtension() -> Int { internalHelper() }
  public static let newerExtensionStatic = 0
}

// A member that is back deployed along with its type needs a body that
// clients can emit.
@backDeployed(before: macOS 99)
@frozen public struct NeverEmitted {
  public var x: Int

  @export(interface) // expected-error {{'@export(interface)' cannot be applied to a back deployed instance method 'interface()'}}
  public func interface() -> Int { x } // expected-note {{instance method 'interface()' is back deployed along with struct 'NeverEmitted' unless it is only available in macOS 99 or newer}}

  // expected-warning@+2 {{'@_neverEmitIntoClient' has been renamed to '@export(interface)'}}
  // expected-error@+1 {{'@_neverEmitIntoClient' cannot be applied to a back deployed instance method 'historical()'}}
  @_neverEmitIntoClient
  public func historical() -> Int { x } // expected-note {{instance method 'historical()' is back deployed along with struct 'NeverEmitted' unless it is only available in macOS 99 or newer}}

  @available(macOS 99, *)
  @export(interface)
  public func newer() -> Int { x }

  @export(interface)
  func notPublic() -> Int { x }
}

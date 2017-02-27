//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An optional type that allows implicit member access.
///
/// *Deprecated.*
@_fixed_layout
public enum ImplicitlyUnwrappedOptional<Wrapped> : ExpressibleByNilLiteral {
  // The compiler has special knowledge of the existence of
  // `ImplicitlyUnwrappedOptional<Wrapped>`, but always interacts with it using
  // the library intrinsics below.

  /// The absence of a value. Typically written using the nil literal, `nil`.
  case none

  /// The presence of a value, stored as `Wrapped`.
  case some(Wrapped)

  /// Creates an instance that stores the given value.
  public init(_ some: Wrapped) { self = .some(some) }

  /// Creates an instance initialized with `nil`.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you initialize an `Optional` instance with a `nil` literal. For example:
  ///
  ///     let i: Index! = nil
  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }
}

extension ImplicitlyUnwrappedOptional : CustomStringConvertible {
  /// A textual representation of the value, or `nil`.
  public var description: String {
    switch self {
    case .some(let value):
      return String(describing: value)
    case .none:
      return "nil"
    }
  }
}

/// Directly conform to CustomDebugStringConvertible to support
/// optional printing. Implementation of that feature relies on
/// _isOptional thus cannot distinguish ImplicitlyUnwrappedOptional
/// from Optional. When conditional conformance is available, this
/// outright conformance can be removed.
extension ImplicitlyUnwrappedOptional : CustomDebugStringConvertible {
  public var debugDescription: String {
    return description
  }
}

#if _runtime(_ObjC)
extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> AnyObject {
    switch self {
    case .none:
      _preconditionFailure("attempt to bridge an implicitly unwrapped optional containing nil")

    case .some(let x):
      return Swift._bridgeAnythingToObjectiveC(x)
    }
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: AnyObject,
    result: inout ImplicitlyUnwrappedOptional<Wrapped>?
  ) {
    result = Swift._forceBridgeFromObjectiveC(x, Wrapped.self)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: AnyObject,
    result: inout ImplicitlyUnwrappedOptional<Wrapped>?
  ) -> Bool {
    let bridged: Wrapped? =
      Swift._conditionallyBridgeFromObjectiveC(x, Wrapped.self)
    if let value = bridged {
      result = value
    }

    return false
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: AnyObject?)
      -> Wrapped! {
    var result: ImplicitlyUnwrappedOptional<Wrapped>?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}
#endif

extension ImplicitlyUnwrappedOptional {
  @available(*, unavailable, message: "Please use nil literal instead.")
  public init() {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Has been removed in Swift 3.")
  public func map<U>(
    _ f: (Wrapped) throws -> U
  ) rethrows -> ImplicitlyUnwrappedOptional<U> {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Has been removed in Swift 3.")
  public func flatMap<U>(
      _ f: (Wrapped) throws -> ImplicitlyUnwrappedOptional<U>
  ) rethrows -> ImplicitlyUnwrappedOptional<U> {
    Builtin.unreachable()
  }
}

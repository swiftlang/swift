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
/// The `ImplicitlyUnwrappedOptional` type is deprecated. To create an optional
/// value that is implicitly unwrapped, place an exclamation mark (`!`) after
/// the type that you want to denote as optional.
///
///     // An implicitly unwrapped optional integer
///     let guaranteedNumber: Int! = 6
///
///     // An optional integer
///     let possibleNumber: Int? = 5
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
  @_inlineable // FIXME(sil-serialize-all)
  public init(_ some: Wrapped) { self = .some(some) }

  /// Creates an instance initialized with `nil`.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you initialize an `Optional` instance with a `nil` literal. For example:
  ///
  ///     let i: Index! = nil
  @_inlineable // FIXME(sil-serialize-all)
  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }
}

#if _runtime(_ObjC)
extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
  @_inlineable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveC() -> AnyObject {
    switch self {
    case .none:
      _preconditionFailure("Attempt to bridge an implicitly unwrapped optional containing nil")

    case .some(let x):
      return Swift._bridgeAnythingToObjectiveC(x)
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func _forceBridgeFromObjectiveC(
    _ x: AnyObject,
    result: inout ImplicitlyUnwrappedOptional<Wrapped>?
  ) {
    result = Swift._forceBridgeFromObjectiveC(x, Wrapped.self)
  }

  @_inlineable // FIXME(sil-serialize-all)
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

  @_inlineable // FIXME(sil-serialize-all)
  public static func _unconditionallyBridgeFromObjectiveC(_ source: AnyObject?)
      -> Wrapped! {
    var result: ImplicitlyUnwrappedOptional<Wrapped>?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}
#endif

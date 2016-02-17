//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An optional type that allows implicit member access.
public enum ImplicitlyUnwrappedOptional<Wrapped>: NilLiteralConvertible {
  // The compiler has special knowledge of the existence of
  // `ImplicitlyUnwrappedOptional<Wrapped>`, but always interacts with it using
  // the library intrinsics below.
  
  case None
  case Some(Wrapped)

  /// Construct a non-`nil` instance that stores `some`.
  public init(_ some: Wrapped) { self = .Some(some) }

  /// Create an instance initialized with `nil`.
  @_transparent public
  init(nilLiteral: ()) {
    self = .None
  }
}

extension ImplicitlyUnwrappedOptional : CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    switch self {
    case .Some(let value):
      return String(value)
    case .None:
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

@_transparent
@warn_unused_result
public // COMPILER_INTRINSIC
func _getImplicitlyUnwrappedOptionalValue<Wrapped>(v: Wrapped!) -> Wrapped {
  switch v {
  case .Some(let x):
    return x
  case .None:
    _requirementFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@_transparent
@warn_unused_result
public // COMPILER_INTRINSIC
func _injectValueIntoImplicitlyUnwrappedOptional<Wrapped>(
  v: Wrapped
) -> Wrapped! {
  return .Some(v)
}

@_transparent
@warn_unused_result
public // COMPILER_INTRINSIC
func _injectNothingIntoImplicitlyUnwrappedOptional<Wrapped>() -> Wrapped! {
  return .None
}

#if _runtime(_ObjC)
extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
  public static func _getObjectiveCType() -> Any.Type {
    return Swift._getBridgedObjectiveCType(Wrapped.self)!
  }

  public func _bridgeToObjectiveC() -> AnyObject {
    switch self {
    case .None:
      _requirementFailure("attempt to bridge an implicitly unwrapped optional containing nil")

    case .Some(let x):
      return Swift._bridgeToObjectiveC(x)!
    }
  }

  public static func _forceBridgeFromObjectiveC(
    x: AnyObject,
    inout result: Wrapped!?
  ) {
    result = Swift._forceBridgeFromObjectiveC(x, Wrapped.self)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    x: AnyObject,
    inout result: Wrapped!?
  ) -> Bool {
    let bridged: Wrapped? =
      Swift._conditionallyBridgeFromObjectiveC(x, Wrapped.self)
    if let value = bridged {
      result = value
    }

    return false
  }

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(Wrapped.self)
  }
}
#endif

extension ImplicitlyUnwrappedOptional {
  @available(*, unavailable, message="Please use nil literal instead.")
  public init() {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="Has been removed in Swift 3.")
  public func map<U>(
    @noescape f: (Wrapped) throws -> U
  ) rethrows -> ImplicitlyUnwrappedOptional<U> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="Has been removed in Swift 3.")
  public func flatMap<U>(
      @noescape f: (Wrapped) throws -> ImplicitlyUnwrappedOptional<U>
  ) rethrows -> ImplicitlyUnwrappedOptional<U> {
    fatalError("unavailable function can't be called")
  }
}

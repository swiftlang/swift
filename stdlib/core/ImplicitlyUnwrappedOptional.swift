//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An optional type that allows implicit member access (via compiler
/// magic).
///
/// The compiler has special knowledge of the existence of
/// ImplicitlyUnwrappedOptional<T>, but always interacts with it using the
/// library intrinsics below.
public enum ImplicitlyUnwrappedOptional<T>
  : Reflectable, NilLiteralConvertible {
  case None
  case Some(T)

  /// Construct a `nil` instance.
  public init() { self = .None }
  
  /// Construct a non-`nil` instance that stores `some`.
  public init(_ some : T) { self = .Some(some) }
  
  /// Construct an instance from an explicitly unwrapped optional
  /// (`T?`).
  public init(_ v : T?) {
    switch v {
    case .Some(let some):
      self = .Some(some)
    case .None:
      self = .None
    }
  }

  /// Create an instance initialized with `nil`.
  @transparent public
  init(nilLiteral: ()) {
    self = .None
  }

  /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
  public func map<U>(@noescape f: (T) -> U) -> ImplicitlyUnwrappedOptional<U> {
    switch self {
    case .Some(let y):
      return .Some(f(y))
    case .None:
      return .None
    }
  }

  /// Returns a mirror that reflects `self`.
  public func getMirror() -> MirrorType {
    // FIXME: This should probably use _OptionalMirror in both cases.
    if let value = self {
      return reflect(value)
    } else {
      return _OptionalMirror(self)
    }
  }
}

extension ImplicitlyUnwrappedOptional : Printable {
  /// A textual representation of `self`.
  public var description: String {
    switch self {
    case .Some(let value):
      return toString(value)
    case .None:
      return "nil"
    }
  }
}

// Intrinsics for use by language features.
@transparent
public // COMPILER_INTRINSIC
func _preconditionImplicitlyUnwrappedOptionalHasValue<T>(inout v: T!) {
  switch v {
  case .Some:
    break
  case .None:
    _preconditionFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@transparent
public // COMPILER_INTRINSIC
func _getImplicitlyUnwrappedOptionalValue<T>(v: T!) -> T {
  switch v {
  case .Some(let x):
    return x
  case .None:
    _preconditionFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@transparent
public // COMPILER_INTRINSIC
func _injectValueIntoImplicitlyUnwrappedOptional<T>(v: T) -> T! {
  return .Some(v)
}

@transparent
public // COMPILER_INTRINSIC
func _injectNothingIntoImplicitlyUnwrappedOptional<T>() -> T! {
  return .None
}

#if _runtime(_ObjC)
extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
  public static func _getObjectiveCType() -> Any.Type {
    return Swift._getBridgedObjectiveCType(T.self)!
  }

  public func _bridgeToObjectiveC() -> AnyObject {
    switch self {
    case .None:
      _preconditionFailure("attempt to bridge an implicitly unwrapped optional containing nil")

    case .Some(let x):
      return Swift._bridgeToObjectiveC(x)!
    }
  }

  public static func _forceBridgeFromObjectiveC(
    x: AnyObject,
    inout result: T!?
  ) {
    result = Swift._forceBridgeFromObjectiveC(x, T.self)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    x: AnyObject,
    inout result: T!?
  ) -> Bool {
    let bridged: T? = Swift._conditionallyBridgeFromObjectiveC(x, T.self)
    if let value = bridged {
      result = value
    }

    return false
  }

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }
}
#endif

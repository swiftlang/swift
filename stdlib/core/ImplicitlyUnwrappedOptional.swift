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
  : BooleanType, Reflectable, NilLiteralConvertible {
  case None
  case Some(T)

  public init() { self = .None }
  public init(_ some : T) { self = .Some(some) }
  public init(_ v : T?) {
    switch v {
    case .Some(let some):
      self = .Some(some)
    case .None:
      self = .None
    }
  }

  // Make nil work with ImplicitlyUnwrappedOptional
  @transparent public
  static func convertFromNilLiteral() -> ImplicitlyUnwrappedOptional<T> {
    return .None
  }

  /// Allow use in a Boolean context.
  @transparent public
  func getLogicValue() -> Bool {
    switch self {
    case .Some:
      return true
    case .None:
      return false
    }
  }

  /// Haskell's fmap, which was mis-named
  public func map<U>(f: (T)->U) -> ImplicitlyUnwrappedOptional<U> {
    switch self {
    case .Some(let y):
      return .Some(f(y))
    case .None:
      return .None
    }
  }

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
@transparent internal
func _doesImplicitlyUnwrappedOptionalHaveValue<T>(inout v: T!) -> Builtin.Int1 {
  return v.getLogicValue().value
}

@transparent internal
func _preconditionImplicitlyUnwrappedOptionalHasValue<T>(inout v: T!) {
  _precondition(v.getLogicValue(),
                "unexpectedly found nil while unwrapping an Optional value")
}

@transparent internal
func _getImplicitlyUnwrappedOptionalValue<T>(v: T!) -> T {
  switch v {
  case .Some(let x):
    return x
  case .None:
    _preconditionFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@transparent internal
func _injectValueIntoImplicitlyUnwrappedOptional<T>(v: T) -> T! {
  return .Some(v)
}

@transparent internal
func _injectNothingIntoImplicitlyUnwrappedOptional<T>() -> T! {
  return .None
}

extension ImplicitlyUnwrappedOptional : _ConditionallyBridgedToObjectiveCType {
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

  public static func _bridgeFromObjectiveC(x: AnyObject) -> T! {
    return Swift._bridgeFromObjectiveC(x, T.self)
  }

  public static func _bridgeFromObjectiveCConditional(x: AnyObject) -> T!? {
    let bridged: T? = Swift._bridgeFromObjectiveCConditional(x, T.self)
    if let value = bridged {
      return value
    }

    return .None
  }

  public static func _isBridgedToObjectiveC() -> Bool {
    return Swift._isBridgedToObjectiveC(T.self)
  }
}

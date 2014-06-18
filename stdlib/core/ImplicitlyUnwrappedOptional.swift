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
/// magic).  We call it 'unchecked' because:
///   - from the user's perspective, it doesn't need an explicit check
///     to use
///   - it's introduced when importing code where the library author
///     hasn't checked whether a type should be null or not
///
/// The compiler has special knowledge of the existence of
/// ImplicitlyUnwrappedOptional<T>, but always interacts with it using the
/// library intrinsics below.
enum ImplicitlyUnwrappedOptional<T>
  : LogicValue, Reflectable, NilLiteralConvertible {
  case None
  case Some(T)

  init() { self = .None }
  init(_ some : T) { self = .Some(some) }
  init(_ v : T?) {
    switch v {
    case .Some(let some):
      self = .Some(some)
    case .None:
      self = .None
    }
  }

  // Make nil work with ImplicitlyUnwrappedOptional
  @transparent
  static func convertFromNilLiteral() -> ImplicitlyUnwrappedOptional<T> {
    return .None
  }

  /// Allow use in a Boolean context.
  @transparent
  func getLogicValue() -> Bool {
    switch self {
    case .Some:
      return true
    case .None:
      return false
    }
  }

  /// Haskell's fmap, which was mis-named
  func map<U>(f: (T)->U) -> ImplicitlyUnwrappedOptional<U> {
    switch self {
    case .Some(let y):
      return .Some(f(y))
    case .None:
      return .None
    }
  }

  func getMirror() -> Mirror {
    // FIXME: This should probably use _OptionalMirror in both cases.
    if let value = self {
      return reflect(value)
    } else {
      return _OptionalMirror(self)
    }
  }
}

extension ImplicitlyUnwrappedOptional : Printable {
  var description: String {
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
func _doesImplicitlyUnwrappedOptionalHaveValue<T>(inout v: T!) -> Builtin.Int1 {
  return v.getLogicValue().value
}

@transparent
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
func _injectValueIntoImplicitlyUnwrappedOptional<T>(v: T) -> T! {
  return .Some(v)
}

@transparent
func _injectNothingIntoImplicitlyUnwrappedOptional<T>() -> T! {
  return .None
}

extension ImplicitlyUnwrappedOptional : _ConditionallyBridgedToObjectiveC {
  typealias ObjectiveCType = AnyObject

  static func getObjectiveCType() -> Any.Type {
    return getBridgedObjectiveCType(T.self)!
  }

  func bridgeToObjectiveC() -> AnyObject {
    switch self {
    case .None:
      _preconditionFailure("attempt to bridge an implicitly unwrapped optional containing nil")

    case .Some(let x):
      return Swift.bridgeToObjectiveC(x)!
    }
  }

  static func bridgeFromObjectiveC(x: AnyObject) -> T!? {
    let bridged: T? = Swift.bridgeFromObjectiveC(x, T.self)
    if let value = bridged {
      return value
    }

    return .None
  }

  static func isBridgedToObjectiveC() -> Bool {
    return Swift.isBridgedToObjectiveC(T.self)
  }
}

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
struct ImplicitlyUnwrappedOptional<T>: LogicValue, Reflectable {
  // Note: explicit initialization to .None is required to break infinite
  // recursion with the otherwise-implicitly-provided default value of
  // `nil`.
  var value: T? = .None

  init() { value = .None }
  init(_ v : T?) { value = v }

  static var None : ImplicitlyUnwrappedOptional {
    @transparent get {
      return ImplicitlyUnwrappedOptional(.None)
    }
  }

  @transparent
  static func Some(value: T) -> ImplicitlyUnwrappedOptional {
    return ImplicitlyUnwrappedOptional(.Some(value))
  }

  /// Allow use in a Boolean context.
  @transparent
  func getLogicValue() -> Bool {
    return value.getLogicValue()
  }

  func getMirror() -> Mirror {
    if let value = self {
      return reflect(value)
    } else {
      return _NilMirror()
    }
  }

  /// Haskell's fmap, which was mis-named
  func map<U>(f: (T)->U) -> ImplicitlyUnwrappedOptional<U> {
    return ImplicitlyUnwrappedOptional<U>(value.map(f))
  }
}

extension ImplicitlyUnwrappedOptional : Printable {
  var description: String {
    return value.description
  }
}

// Intrinsics for use by language features.
@transparent
func _doesImplicitlyUnwrappedOptionalHaveValue<T>(inout v: T!) -> Builtin.Int1 {
  return _doesOptionalHaveValue(&v.value)
}

@transparent
func _getImplicitlyUnwrappedOptionalValue<T>(v: T!) -> T {
  return _getOptionalValue(v.value)
}

@transparent
func _injectValueIntoImplicitlyUnwrappedOptional<T>(v: T) -> T! {
  return ImplicitlyUnwrappedOptional(_injectValueIntoOptional(v))
}

@transparent
func _injectNothingIntoImplicitlyUnwrappedOptional<T>() -> T! {
  return ImplicitlyUnwrappedOptional(_injectNothingIntoOptional())
}

// Make nil work with ImplicitlyUnwrappedOptional (and Optional, via subtype conversion).
extension _Nil {
  @conversion func __conversion<T>() -> T! {
    return .None
  }
}

extension ImplicitlyUnwrappedOptional : _ConditionallyBridgedToObjectiveC {
  typealias ObjectiveCType = AnyObject

  static func getObjectiveCType() -> Any.Type {
    return getBridgedObjectiveCType(T.self)!
  }

  func bridgeToObjectiveC() -> AnyObject {
    switch self.value {
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

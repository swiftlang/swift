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

// The compiler has special knowledge of Optional<Wrapped>, including the fact
// that it is an enum with cases named 'None' and 'Some'.
public enum Optional<Wrapped> : _Reflectable, NilLiteralConvertible {
  case None
  case Some(Wrapped)

  @available(*, unavailable, renamed="Wrapped")
  public typealias T = Wrapped

  /// Construct a `nil` instance.
  @_transparent
  public init() { self = .None }

  /// Construct a non-`nil` instance that stores `some`.
  @_transparent
  public init(_ some: Wrapped) { self = .Some(some) }

  /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
  @warn_unused_result
  public func map<U>(@noescape f: (Wrapped) throws -> U) rethrows -> U? {
    switch self {
    case .Some(let y):
      return .Some(try f(y))
    case .None:
      return .None
    }
  }

  /// Returns `nil` if `self` is `nil`, `f(self!)` otherwise.
  @warn_unused_result
  public func flatMap<U>(@noescape f: (Wrapped) throws -> U?) rethrows -> U? {
    switch self {
    case .Some(let y):
      return try f(y)
    case .None:
      return .None
    }
  }

  /// Returns a mirror that reflects `self`.
  @warn_unused_result
  public func _getMirror() -> _MirrorType {
    return _OptionalMirror(self)
  }

  /// Create an instance initialized with `nil`.
  @_transparent
  public init(nilLiteral: ()) {
    self = .None
  }
}

extension Optional : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    switch self {
    case .Some(let value):
      var result = "Optional("
      debugPrint(value, terminator: "", toStream: &result)
      result += ")"
      return result
    case .None:
      return "nil"
    }
  }
}

// While this free function may seem obsolete, since an optional is
// often expressed as (x as Wrapped), it can lead to cleaner usage, i.e.
//
//   map(x as Wrapped) { ... }
// vs
//   (x as Wrapped).map { ... }
//
/// Haskell's fmap for Optionals.
@available(*, unavailable, message="call the 'map()' method on the optional value")
public func map<T, U>(x: T?, @noescape _ f: (T)->U) -> U? {
  fatalError("unavailable function can't be called")
}


/// Returns `f(self)!` iff `self` and `f(self)` are not `nil`.
@available(*, unavailable, message="call the 'flatMap()' method on the optional value")
public func flatMap<T, U>(x: T?, @noescape _ f: (T)->U?) -> U? {
  fatalError("unavailable function can't be called")
}

// Intrinsics for use by language features.
@_transparent
public // COMPILER_INTRINSIC
func _doesOptionalHaveValueAsBool<Wrapped>(v: Wrapped?) -> Bool {
  return v != nil
}

@_transparent
public // COMPILER_INTRINSIC
func _diagnoseUnexpectedNilOptional() {
  _preconditionFailure(
                "unexpectedly found nil while unwrapping an Optional value")
}

@_transparent
public // COMPILER_INTRINSIC
func _getOptionalValue<Wrapped>(v: Wrapped?) -> Wrapped {
  switch v {
  case let x?:
    return x
  case .None:
    _preconditionFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@_transparent
public // COMPILER_INTRINSIC
func _injectValueIntoOptional<Wrapped>(v: Wrapped) -> Wrapped? {
  return .Some(v)
}

@_transparent
public // COMPILER_INTRINSIC
func _injectNothingIntoOptional<Wrapped>() -> Wrapped? {
  return .None
}

// Comparisons
@warn_unused_result
public func == <T: Equatable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l == r
  case (nil, nil):
    return true
  default:
    return false
  }
}

@warn_unused_result
public func != <T : Equatable> (lhs: T?, rhs: T?) -> Bool {
  return !(lhs == rhs)
}

// Enable pattern matching against the nil literal, even if the element type
// isn't equatable.
public struct _OptionalNilComparisonType : NilLiteralConvertible {
  /// Create an instance initialized with `nil`.
  @_transparent
  public init(nilLiteral: ()) {
  }
}
@_transparent
@warn_unused_result
public func ~= <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .Some(_):
    return false
  case .None:
    return true
  }
}

// Enable equality comparisons against the nil literal, even if the
// element type isn't equatable
@warn_unused_result
public func == <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .Some(_):
    return false
  case .None:
    return true
  }
}

@warn_unused_result
public func != <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .Some(_):
    return true
  case .None:
    return false
  }
}

@warn_unused_result
public func == <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .Some(_):
    return false
  case .None:
    return true
  }
}

@warn_unused_result
public func != <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .Some(_):
    return true
  case .None:
    return false
  }
}

internal struct _OptionalMirror<Wrapped> : _MirrorType {
  let _value : Optional<Wrapped>

  init(_ x : Optional<Wrapped>) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return (_value != nil) ? 1 : 0 }

  subscript(i: Int) -> (String, _MirrorType) {
    switch (_value, i) {
    case (.Some(let contents), 0) : return ("Some", _reflect(contents))
    default: _preconditionFailure("cannot extract this child index")
    }
  }

  var summary: String {
    switch _value {
      case let contents?: return _reflect(contents).summary
      default: return "nil"
    }
  }

  var quickLookObject: PlaygroundQuickLook? { return .None }

  var disposition: _MirrorDisposition { return .Optional }
}


@warn_unused_result
public func < <T : Comparable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l < r
  case (nil, _?):
    return true
  default:
    return false
  }
}

@warn_unused_result
public func > <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l > r
  default:
    return rhs < lhs
  }
}

@warn_unused_result
public func <= <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l <= r
  default:
    return !(rhs < lhs)
  }
}

@warn_unused_result
public func >= <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l >= r
  default:
    return !(lhs < rhs)
  }
}

@_transparent
@warn_unused_result
public func ?? <T> (optional: T?, @autoclosure defaultValue: () throws -> T)
    rethrows -> T {
  switch optional {
  case .Some(let value):
    return value
  case .None:
    return try defaultValue()
  }
}

@_transparent
@warn_unused_result
public func ?? <T> (optional: T?, @autoclosure defaultValue: () throws -> T?)
    rethrows -> T? {
  switch optional {
  case .Some(let value):
    return value
  case .None:
    return try defaultValue()
  }
}

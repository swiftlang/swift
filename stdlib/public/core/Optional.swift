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

// The compiler has special knowledge of Optional<T>, including the fact that
// it is an enum with cases named 'None' and 'Some'.
public enum Optional<T> : Reflectable, NilLiteralConvertible {
  case None
  case Some(T)

  /// Construct a `nil` instance.
  @transparent
  public init() { self = .None }

  /// Construct a non-\ `nil` instance that stores `some`.
  @transparent
  public init(_ some: T) { self = .Some(some) }

  /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
  public func map<U>(@noescape f: (T)->U) -> U? {
    switch self {
    case .Some(var y):
      return .Some(f(y))
    case .None:
      return .None
    }
  }

  /// Returns `f(self)!` iff `self` and `f(self)` are not nil.
  public func flatMap<U>(@noescape f: (T)->U?) -> U? {
    switch self {
    case .Some(let y):
      switch f(y) {
      case .Some(let z):
        return .Some(z)
      case .None:
        return .None
      }
    case .None:
      return .None
    }
  }

  /// Returns a mirror that reflects `self`.
  public func getMirror() -> MirrorType {
    return _OptionalMirror(self)
  }

  /// Create an instance initialized with `nil`.
  @transparent
  public init(nilLiteral: ()) {
    self = .None
  }
}

extension Optional : DebugPrintable {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    switch self {
    case .Some(var value):
      var result = "Optional("
      debugPrint(value, &result)
      result += ")"
      return result
    case .None:
      return "nil"
    }
  }
}

// While this free function may seem obsolete, since an optional is
// often expressed as (x as T), it can lead to cleaner usage, i.e.
//
//   map(x as T) { ... }
// vs
//   (x as T).map { ... }
//
/// Haskell's fmap for Optionals.
public func map<T, U>(x: T?, @noescape f: (T)->U) -> U? {
  switch x {
    case .Some(var y):
    return .Some(f(y))
    case .None:
    return .None
  }
}


/// Returns `f(self)!` iff `self` and `f(self)` are not nil.
public func flatMap<T, U>(x: T?, @noescape f: (T)->U?) -> U? {
  switch x {
  case .Some(let y):
    switch f(y) {
    case .Some(let z):
      return .Some(z)
    case .None:
      return .None
    }
  case .None:
    return .None
  }
}

// Intrinsics for use by language features.
@transparent
public // COMPILER_INTRINSIC
func _doesOptionalHaveValueAsBool<T>(v: T?) -> Bool {
  return v != nil
}

@transparent
public // COMPILER_INTRINSIC
func _diagnoseUnexpectedNilOptional() {
  _preconditionFailure(
                "unexpectedly found nil while unwrapping an Optional value")
}

@transparent
public // COMPILER_INTRINSIC
func _getOptionalValue<T>(v: T?) -> T {
  switch v {
  case .Some(var x):
    return x
  case .None:
    _preconditionFailure(
      "unexpectedly found nil while unwrapping an Optional value")
  }
}

@transparent
public // COMPILER_INTRINSIC
func _injectValueIntoOptional<T>(v: T) -> T? {
  return .Some(v)
}

@transparent
public // COMPILER_INTRINSIC
func _injectNothingIntoOptional<T>() -> T? {
  return .None
}

// Comparisons
public func == <T : Equatable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l == r
  case (.None, .None):
    return true
  default:
    return false
  }
}

public func != <T : Equatable> (lhs: T?, rhs: T?) -> Bool {
  return !(lhs == rhs)
}

// Enable pattern matching against the nil literal, even if the element type
// isn't equatable.
public struct _OptionalNilComparisonType : NilLiteralConvertible {
  /// Create an instance initialized with `nil`.
  @transparent
  public init(nilLiteral: ()) {
  }
}
@transparent
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
public func == <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .Some(_):
    return false
  case .None:
    return true
  }
}

public func != <T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool {
  switch lhs {
  case .Some(_):
    return true
  case .None:
    return false
  }
}

public func == <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .Some(_):
    return false
  case .None:
    return true
  }
}

public func != <T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool {
  switch rhs {
  case .Some(_):
    return true
  case .None:
    return false
  }
}

internal struct _OptionalMirror<T> : MirrorType {
  let _value : Optional<T>

  init(_ x : Optional<T>) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return (_value != nil) ? 1 : 0 }

  subscript(i: Int) -> (String, MirrorType) {
    switch (_value,i) {
    case (.Some(let contents),0) : return ("Some",reflect(contents))
    default: _preconditionFailure("cannot extract this child index")
    }
  }

  var summary: String {
    switch _value {
      case .Some(let contents): return reflect(contents).summary
      default: return "nil"
    }
  }

  var quickLookObject: QuickLookObject? { return .None }

  var disposition: MirrorDisposition { return .Optional }
}


public func < <T: _Comparable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l < r
  case (.None, .Some):
    return true
  default:
    return false
  }
}

public func > <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l > r
  default:
    return rhs < lhs
  }
}

public func <= <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l <= r
  default:
    return !(rhs < lhs)
  }
}

public func >= <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l >= r
  default:
    return !(lhs < rhs)
  }
}

@transparent
public func ?? <T> (optional: T?, @autoclosure defaultValue: () -> T) -> T {
  switch optional {
  case .Some(let value):
    return value
  case .None:
    return defaultValue()
  }
}

@transparent
public func ?? <T> (optional: T?, @autoclosure defaultValue: () -> T?) -> T? {
  switch optional {
  case .Some(let value):
    return value
  case .None:
    return defaultValue()
  }
}

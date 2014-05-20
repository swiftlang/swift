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
enum Optional<T>: LogicValue, Reflectable {
  case None
  case Some(T)

  init() { self = .None }

  init(_ some: T) { self = .Some(some) }

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
  func map<U>(f: (T)->U) -> U? {
    switch self {
    case .Some(var y):
      return .Some(f(y))
    case .None:
      return .None
    }
  }

  func getMirror() -> Mirror {
    return _OptionalMirror(self)
  }
}

extension Optional : Printable {
  var description: String {
    switch self {
    case .Some(var value):
      return toString(value)
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
func map<T, U>(x: T?, f: (T)->U) -> U? {
  switch x {
    case .Some(var y):
    return .Some(f(y))
    case .None:
    return .None
  }
}

// Intrinsics for use by language features.
@transparent
func _doesOptionalHaveValue<T>(inout v: T?) -> Builtin.Int1 {
  return v.getLogicValue().value
}

@transparent
func _getOptionalValue<T>(v: T?) -> T {
  switch v {
  case .Some(var x):
    return x
  case .None:
    _preconditionFailure("Can't unwrap Optional.None")
  }
}

@transparent
func _injectValueIntoOptional<T>(v: T) -> Optional<T> {
  return .Some(v)
}

@transparent
func _injectNothingIntoOptional<T>() -> Optional<T> {
  return .None
}

// Comparisons
func == <T: Equatable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l == r
  case (.None, .None):
    return true
  default:
    return false
  }
}

func != <T: Equatable> (lhs: T?, rhs: T?) -> Bool {
  return !(lhs == rhs)
}

@transparent
func == <T>(lhs: T?, rhs: _Nil) -> Bool {
  return lhs.getLogicValue() == false
}

@transparent
func == <T>(lhs: _Nil, rhs: T?) -> Bool {
  return rhs.getLogicValue() == false
}

@transparent
func != <T>(lhs: T?, rhs: _Nil) -> Bool {
  return !(lhs == rhs)
}

@transparent
func != <T>(lhs: _Nil, rhs: T?) -> Bool {
  return !(lhs == rhs)
}

struct _OptionalMirror<T> : Mirror {
  let _value : Optional<T>

  init(_ x : Optional<T>) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return _value ? 1 : 0 }

  subscript(i: Int) -> (String, Mirror) { 
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


// FIXME: ordering comparison of Optionals disabled pending
func < <T: _Comparable> (lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l < r
  case (.None, .Some):
    return true
  default:
    return false
  }
}

func > <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l > r
  default:
    return rhs < lhs
  }
}

func <= <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l <= r
  default:
    return !(rhs < lhs)
  }
}

func >= <T: _Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs,rhs) {
  case (.Some(let l), .Some(let r)):
    return l >= r
  default:
    return !(lhs < rhs)
  }
}

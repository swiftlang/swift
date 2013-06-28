/* Optional<T>

   Until we have working oneof types, we have this library-based oneof.

*/

// Users should never touch this type directly.
struct __NoneType {
  func getLogicValue() -> Bool { return false }
}
func [prefix] !(x: __NoneType) -> Bool { return true }

extension __NoneType : Equatable {
  func __equal__(rhs: __NoneType) -> Bool { 
    return true 
  }
}

// This constant is for public consumption
var None: __NoneType {
  return __NoneType()
}

/*

//
// FIXME: Until Swift supports partial ordering of generic functions,
// these will cause ambiguities against more useful overloads like
// those interactions with Optional<T>.  Keep them on ice until then.
//
func == <T>(lhs: __NoneType, rhs: T) -> Bool {
  return rhs.isNone()
}

func != <T>(lhs: __NoneType, rhs: Optional<T>) -> Bool {
  return !rhs.isNone()
}

func == <T>(lhs: Optional<T>, rhs: __NoneType) -> Bool {
  return lhs.isNone()
}

func != <T>(lhs: Optional<T>, rhs: __NoneType) -> Bool {
  return !lhs.isNone()
}

*/

struct Optional<T>: Enumerable, LogicValue {
  typealias EnumeratorType = Slice<T>
  func getEnumeratorType() -> Slice<T> { return value }

  constructor() {}

  constructor(x: T) {
    value = new T[1]
    value[0] = x
  }

  constructor(x: __NoneType) {}

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value.length > 0
  }

  func isNone() -> Bool {
    return !getLogicValue()
  }

  func get() -> T {
    assert(value.length > 0)
    return value[0]
  }

  var value: T[]
}

// Emulate .Some(x) tag constructor to be delivered by oneof
func Some<T>(x: T) -> Optional<T> {
  return Optional(x)
}

// FIXME: Has no effect pending <rdar://problem/13785669>
extension __NoneType {
  func [conversion] __conversion<T> () -> Optional<T> {
    return Optional(None)
  }
}

func [prefix] ! <T>(x: Optional<T>) -> Bool { 
  return !x.getLogicValue()
}

func == <T>(lhs: __NoneType, rhs: Optional<T>) -> Bool {
  return rhs.isNone()
}

func != <T>(lhs: __NoneType, rhs: Optional<T>) -> Bool {
  return !rhs.isNone()
}

func == <T>(lhs: Optional<T>, rhs: __NoneType) -> Bool {
  return lhs.isNone()
}

func != <T>(lhs: Optional<T>, rhs: __NoneType) -> Bool {
  return !lhs.isNone()
}

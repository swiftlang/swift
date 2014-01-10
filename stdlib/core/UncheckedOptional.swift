// An optional type that allows implicit member access (via compiler
// magic).  We call it 'unchecked' because:
//   - from the user's perspective, it doesn't need an explicit check
//     to use
//   - it's introduced when importing code where the library author
//     hasn't checked whether a type should be null or not
//
// The compiler has special knowledge of the existence of
// UncheckedOptional<T>, but always interacts with it using the
// library intrinsics below.
struct UncheckedOptional<T>: LogicValue {
  var value: T?

  init() { value = .None }
  init(v : T?) { value = v }

  /// \brief Allow use in a Boolean context.
  @transparent
  func getLogicValue() -> Bool {
    return value.getLogicValue()
  }

  /// \brief Haskell's fmap, which was mis-named
  func map<U>(f: (T)->U) -> UncheckedOptional<U> {
    return UncheckedOptional<U>(value.map(f))
  }
}

// While this free function may seem obsolete, since an optional is
// often expressed as (x as T), it can lead to cleaner usage, i.e.
//
//   map(x as T) { ... }
// vs
//   (x as T).map { ... }
//
/// \brief Haskell's fmap for Optionals.
func map<T, U>(x: UncheckedOptional<T>, f: (T)->U) -> UncheckedOptional<U> {
  return x.map(f)
}

// Intrinsics for use by language features.
@transparent
func _convertUncheckedOptionalToOptional<T>(v: UncheckedOptional<T>) -> T? {
  return v.value
}

@transparent
func _convertOptionalToUncheckedOptional<T>(v: T?) -> UncheckedOptional<T> {
  return UncheckedOptional(v)
}

@transparent
func _getUncheckedOptionalValue<T>(v: UncheckedOptional<T>) -> T {
  return _getOptionalValue(v.value)
}

@transparent
func _injectValueIntoUncheckedOptional<T>(v: T) -> UncheckedOptional<T> {
  return UncheckedOptional(_injectValueIntoOptional(v))
}

@transparent
func _injectNothingIntoUncheckedOptional<T>() -> UncheckedOptional<T> {
  return UncheckedOptional(_injectNothingIntoOptional())
}

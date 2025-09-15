// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

/// Used to verify the type of an expression. Use like this:
/// ```
/// var types = SwiftTypePair(typeOf: expr, type2: SwiftType<Int>.self)
/// types.assertTypesAreEqual()
/// ```
struct SwiftType<T> {}
struct SwiftTypePair<T1, T2> {
  init(typeOf: T1, type2: SwiftType<T2>.Type) {}

  mutating func assertTypesAreEqual() where T1 == T2 {}
}
protocol P {}
extension P {
  func method() -> some P { self }
  var property: some P { self }
  subscript() -> some P { self }
}

do {
  let exist: any P

  do {
    var types = SwiftTypePair(typeOf: exist.method(), type2: SwiftType<any P>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.property, type2: SwiftType<any P>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist[], type2: SwiftType<any P>.self)
    types.assertTypesAreEqual()
  }
}

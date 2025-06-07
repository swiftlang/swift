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

class ClassBase {}
class ClassDerived<T>: ClassBase {}

protocol CovariantAssocTypeErasure {
  associatedtype A1
  associatedtype A2: AnyObject
  associatedtype A3: CovariantAssocTypeErasure
  associatedtype A4: ClassBase
  associatedtype A5: ClassDerived<Self>
  associatedtype A6: CovariantAssocTypeErasure & ClassBase
  associatedtype A7: ClassDerived<Self> & CovariantAssocTypeErasure

  associatedtype B1 where B1 == Optional<A1>
  associatedtype B2 where B2 == (A2, Bool)
  associatedtype B3 where B3 == A3.Type
  associatedtype B4 where B4 == Array<A4>
  associatedtype B5 where B5 == Dictionary<String, A5>

  func method1() -> A1
  func method2() -> A2
  func method3() -> A3
  func method4() -> A4
  func method5() -> A5
  func method6() -> A6
  func method7() -> A7

  func method8() -> B1
  func method9() -> B2
  func method10() -> B3
  func method11() -> B4
  func method12() -> B5
}
protocol CovariantAssocTypeErasureDerived: CovariantAssocTypeErasure
where A1: CovariantAssocTypeErasureDerived,
      A2: ClassBase,
      A3: CovariantAssocTypeErasureDerived,
      A4: CovariantAssocTypeErasureDerived,
      A5: CovariantAssocTypeErasureDerived,
      A6: CovariantAssocTypeErasureDerived,
      A7: Sequence {}
do {
  let exist: any CovariantAssocTypeErasure

  do {
    var types = SwiftTypePair(typeOf: exist.method1(), type2: SwiftType<Any>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method2(), type2: SwiftType<AnyObject>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method3(),
      type2: SwiftType<any CovariantAssocTypeErasure>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method4(), type2: SwiftType<ClassBase>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method5(), type2: SwiftType<ClassBase>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method6(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasure>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method7(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasure>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method8(), type2: SwiftType<Any?>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method9(), type2: SwiftType<(AnyObject, Bool)>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method10(),
      type2: SwiftType<any CovariantAssocTypeErasure.Type>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method11(), type2: SwiftType<Array<ClassBase>>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method12(),
      type2: SwiftType<Dictionary<String, ClassBase>>.self
    )
    types.assertTypesAreEqual()
  }

  let _ = exist.method1()
  let _ = exist.method2()
  let _ = exist.method3()
  let _ = exist.method4()
  let _ = exist.method5()
  let _ = exist.method6()
  let _ = exist.method7()
  let _ = exist.method8()
  let _ = exist.method9()
  let _ = exist.method10()
  let _ = exist.method11()
  let _ = exist.method12()
}
do {
  let exist: any CovariantAssocTypeErasureDerived

  do {
    var types = SwiftTypePair(
      typeOf: exist.method1(),
      type2: SwiftType<any CovariantAssocTypeErasureDerived>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method2(), type2: SwiftType<ClassBase>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method3(),
      type2: SwiftType<any CovariantAssocTypeErasureDerived>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method4(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasureDerived>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method5(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasureDerived>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method6(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasureDerived>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method7(),
      type2: SwiftType<any ClassBase & CovariantAssocTypeErasure & Sequence>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method8(),
      type2: SwiftType<(any CovariantAssocTypeErasureDerived)?>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.method9(), type2: SwiftType<(ClassBase, Bool)>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method10(),
      type2: SwiftType<any CovariantAssocTypeErasureDerived.Type>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method11(),
      type2: SwiftType<Array<any ClassBase & CovariantAssocTypeErasureDerived>>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.method12(),
      type2: SwiftType<Dictionary<String, any ClassBase & CovariantAssocTypeErasureDerived>>.self
    )
    types.assertTypesAreEqual()
  }
}

// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

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

  let _: Any = exist.method1()
  let _: AnyObject = exist.method2()
  let _: any CovariantAssocTypeErasure = exist.method3()
  let _: ClassBase = exist.method4()
  let _: ClassBase = exist.method5()
  let _: any ClassBase & CovariantAssocTypeErasure = exist.method6()
  let _: any ClassBase & CovariantAssocTypeErasure = exist.method7()
  let _: Any? = exist.method8()
  let _: (AnyObject, Bool) = exist.method9()
  let _: any CovariantAssocTypeErasure.Type = exist.method10()
  let _: Array<ClassBase> = exist.method11()
  let _: Dictionary<String, ClassBase> = exist.method12()

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

  let _: any CovariantAssocTypeErasureDerived = exist.method1()
  let _: ClassBase = exist.method2()
  let _: any CovariantAssocTypeErasureDerived = exist.method3()
  let _: any ClassBase & CovariantAssocTypeErasureDerived = exist.method4()
  let _: any ClassBase & CovariantAssocTypeErasureDerived = exist.method5()
  let _: any ClassBase & CovariantAssocTypeErasureDerived = exist.method6()
  let _: any ClassBase & CovariantAssocTypeErasure & Sequence = exist.method7()

  let _: (any CovariantAssocTypeErasureDerived)? = exist.method8()
  let _: (ClassBase, Bool) = exist.method9()
  let _: any CovariantAssocTypeErasureDerived.Type = exist.method10()
  let _: Array<any ClassBase & CovariantAssocTypeErasureDerived> = exist.method11()
  let _: Dictionary<String, any ClassBase & CovariantAssocTypeErasureDerived> = exist.method12()
}

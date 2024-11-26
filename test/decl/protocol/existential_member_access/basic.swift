// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct Struct<T> {
  class Inner {}
  struct InnerGeneric<U> {}
}
class Class<T> {}

/// Used to match a type to the inferred type of an expression.
///
/// Use like this:
/// ```
/// let assertion = SwiftType<Int>.IsStaticTypeOfExprAssertion(true);
/// assertion.assert()
/// ```
///
/// Not like this: `SwiftType<Int>.IsStaticTypeOfExprAssertion(true).assert()`!
struct SwiftType<T> {
  struct IsStaticTypeOfExprAssertion<U> {
    init(_: U) {}

    func assert() where T == U {}
  }
}

// Covariant references.
do {
  protocol P {
    associatedtype A

    func covariantSelf1() -> Self
    func covariantSelf2() -> Self?
    func covariantSelf3() -> Self.Type
    func covariantSelf4() -> (Self, Self)
    func covariantSelf5() -> Array<Self>
    func covariantSelf6() -> [String : Self]
    func covariantSelf7(_: (Self) -> Void)
    func covariantSelf8(_: (Self...) -> Void)
    func covariantSelf9() -> Self?.Type
    func covariantSelfComplex(_: (Self.Type) -> Void,
                              _: (Self.Type...) -> Void,
                              _: (Array<Self>) -> Void,
                              _: (Array<Array<Self>?>) -> Void,
                              _: (() -> Self?) -> Void
                             ) -> [String : () -> (Self, Self)]
    func covariantAssoc1() -> A
    func covariantAssoc2() -> A?
    func covariantAssoc3() -> A.Type
    func covariantAssoc4() -> (A, A)
    func covariantAssoc5() -> Array<A>
    func covariantAssoc6() -> [String : A]
    func covariantAssoc7(_: (A) -> Void)
    func covariantAssoc8(_: (A...) -> Void)
    func covariantAssoc9() -> A?.Type
    func covariantAssocComplex(_: (A.Type) -> Void,
                               _: (A.Type...) -> Void,
                               _: (Array<A>) -> Void,
                               _: (Array<Array<A>?>) -> Void,
                               _: (() -> A?) -> Void
                              ) -> [String : () -> (A, A)]


    var covariantSelfProp1: Self { get }
    var covariantSelfProp2: Self? { get }
    var covariantSelfProp3: Self.Type { get }
    var covariantSelfProp4: (Self, Self) { get }
    var covariantSelfProp5: Array<Self> { get }
    var covariantSelfProp6: [String : Self] { get }
    var covariantSelfProp7: ((Self) -> Void) -> Void { get }
    var covariantSelfProp8: ((Self...) -> Void) -> Void { get }
    var covariantSelfPropComplex: ((Self.Type) -> Void,
                                   (Self.Type...) -> Void,
                                   (Array<Self>) -> Void,
                                   (Array<Array<Self>?>) -> Void,
                                   (() -> Self?) -> Void
                                  ) -> [String : () -> (Self, Self)] { get }
    var covariantAssocProp1: A { get }
    var covariantAssocProp2: A? { get }
    var covariantAssocProp3: A.Type { get }
    var covariantAssocProp4: (A, A) { get }
    var covariantAssocProp5: Array<A> { get }
    var covariantAssocProp6: [String : A] { get }
    var covariantAssocProp7: ((A) -> Void) -> Void { get }
    var covariantAssocProp8: ((A...) -> Void) -> Void { get }
    var covariantAssocPropComplex: ((A.Type) -> Void,
                                    (A.Type...) -> Void,
                                    (Array<A>) -> Void,
                                    (Array<Array<A>?>) -> Void,
                                    (() -> A?) -> Void
                                   ) -> [String : () -> (A, A)] { get }

    // Subscripts
    subscript(covariantSelfSubscript1 _: Void) -> Self { get }
    subscript(covariantSelfSubscript2 _: Void) -> Self? { get }
    subscript(covariantSelfSubscript3 _: Void) -> Self.Type { get }
    subscript(covariantSelfSubscript4 _: Void) -> (Self, Self) { get }
    subscript(covariantSelfSubscript5 _: Void) -> Array<Self> { get }
    subscript(covariantSelfSubscript6 _: Void) -> [String : Self] { get }
    subscript(covariantSelfSubscript7 _: (Self) -> Void) -> Self { get }
    subscript(covariantSelfSubscript8 _: (Self...) -> Void) -> Self { get }
    subscript(covariantSelfSubscriptComplex
              _: (Self.Type) -> Void,
              _: (Self.Type...) -> Void,
              _: (Array<Self>) -> Void,
              _: (Array<Array<Self>?>) -> Void,
              _: (() -> Self?) -> Void
              ) -> [String : () -> (Self, Self)] { get }
    subscript(covariantAssocSubscript1 _: Void) -> A { get }
    subscript(covariantAssocSubscript2 _: Void) -> A? { get }
    subscript(covariantAssocSubscript3 _: Void) -> A.Type { get }
    subscript(covariantAssocSubscript4 _: Void) -> (A, A) { get }
    subscript(covariantAssocSubscript5 _: Void) -> Array<A> { get }
    subscript(covariantAssocSubscript6 _: Void) -> [String : A] { get }
    subscript(covariantAssocSubscript7 _: (A) -> Void) -> A { get }
    subscript(covariantAssocSubscript8 _: (A...) -> Void) -> Self { get }
    subscript(covariantAssocSubscriptComplex
              _: (A.Type) -> Void,
              _: (A.Type...) -> Void,
              _: (Array<A>) -> Void,
              _: (Array<Array<A>?>) -> Void,
              _: (() -> A?) -> Void
              ) -> [String : () -> (A, A)] { get }
  }

  let exist: any P

  let _: any P = exist.covariantSelf1()
  let _: (any P)? = exist.covariantSelf2()
  let _: any P.Type = exist.covariantSelf3()
  let _: (any P, any P) = exist.covariantSelf4()
  let _: Array<any P> = exist.covariantSelf5()
  let _: [String : any P] = exist.covariantSelf6()
  exist.covariantSelf7 { (_: any P) in }
  exist.covariantSelf8 { (_: any P...) in }
  do {
    // FIXME: Because B?.Type to A?.Type upcast is not supported.
    // expected-error@+1 {{member 'covariantSelf9()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let a = SwiftType<(any P)?.Type>.IsStaticTypeOfExprAssertion(exist.covariantSelf9())
    a.assert()
  }
  let _: [String : () -> (any P, any P)] = exist.covariantSelfComplex(
    { (_: any P.Type) in },
    { (_: any P.Type...) in },
    { (_: Array<any P>) in },
    { (_: Array<Array<any P>?>) in },
    { (_: () -> (any P)?) in }
  )

  let _: Any = exist.covariantAssoc1()
  let _: Any? = exist.covariantAssoc2()
  let _: Any.Type = exist.covariantAssoc3()
  let _: (Any, Any) = exist.covariantAssoc4()
  let _: Array<Any> = exist.covariantAssoc5()
  let _: [String : Any] = exist.covariantAssoc6()
  exist.covariantAssoc7 { (_: Any) in }
  exist.covariantAssoc8 { (_: Any...) in }
  do {
    // FIXME: Because B?.Type to A?.Type upcast is not supported.
    // expected-error@+1 {{member 'covariantAssoc9()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let a = SwiftType<Any?.Type>.IsStaticTypeOfExprAssertion(exist.covariantAssoc9())
    a.assert()
  }
  let _: [String : () -> (Any, Any)] = exist.covariantAssocComplex(
    { (_: Any.Type) in },
    { (_: Any.Type...) in },
    { (_: Array<Any>) in },
    { (_: Array<Array<Any>?>) in },
    { (_: () -> Any?) in }
  )

  let _: any P = exist.covariantSelfProp1
  let _: (any P)? = exist.covariantSelfProp2
  let _: any P.Type = exist.covariantSelfProp3
  let _: (any P, any P) = exist.covariantSelfProp4
  let _: Array<any P> = exist.covariantSelfProp5
  let _: [String : any P] = exist.covariantSelfProp6
  let _: ((any P) -> Void) -> Void = exist.covariantSelfProp7
  let _: ((any P...) -> Void) -> Void = exist.covariantSelfProp8
  let _: (
    (any P.Type) -> Void,
    (any P.Type...) -> Void,
    (Array<any P>) -> Void,
    (Array<Array<any P>?>) -> Void,
    (() -> (any P)?) -> Void
  ) -> [String : () -> (any P, any P)] = exist.covariantSelfPropComplex

  let _: Any = exist.covariantAssocProp1
  let _: Any? = exist.covariantAssocProp2
  let _: Any.Type = exist.covariantAssocProp3
  let _: (Any, Any) = exist.covariantAssocProp4
  let _: Array<Any> = exist.covariantAssocProp5
  let _: [String : Any] = exist.covariantAssocProp6
  let _: ((Any) -> Void) -> Void = exist.covariantAssocProp7
  let _: ((Any...) -> Void) -> Void = exist.covariantAssocProp8
  let _: (
    (Any.Type) -> Void,
    (Any.Type...) -> Void,
    (Array<Any>) -> Void,
    (Array<Array<Any>?>) -> Void,
    (() -> Any?) -> Void
  ) -> [String : () -> (Any, Any)] = exist.covariantAssocPropComplex

  let _: any P = exist[covariantSelfSubscript1: ()]
  let _: (any P)? = exist[covariantSelfSubscript2: ()]
  let _: any P.Type = exist[covariantSelfSubscript3: ()]
  let _: (any P, any P) = exist[covariantSelfSubscript4: ()]
  let _: Array<any P> = exist[covariantSelfSubscript5: ()]
  let _: [String : any P] = exist[covariantSelfSubscript6: ()]
  let _: any P = exist[covariantSelfSubscript7: { (_: any P) in }]
  let _: any P = exist[covariantSelfSubscript8: { (_: any P...) in }]
  let _: [String : () -> (any P, any P)] = exist[
    covariantSelfSubscriptComplex: { (_: any P.Type) in },
    { (_: any P.Type...) in },
    { (_: Array<any P>) in },
    { (_: Array<Array<any P>?>) in },
    { (_: () -> (any P)?) in }
  ]

  let _: Any = exist[covariantAssocSubscript1: ()]
  let _: Any? = exist[covariantAssocSubscript2: ()]
  let _: Any.Type = exist[covariantAssocSubscript3: ()]
  let _: (Any, Any) = exist[covariantAssocSubscript4: ()]
  let _: Array<Any> = exist[covariantAssocSubscript5: ()]
  let _: [String : Any] = exist[covariantAssocSubscript6: ()]
  let _: Any = exist[covariantAssocSubscript7: { (_: Any) in }]
  let _: Any = exist[covariantAssocSubscript8: { (_: Any...) in }]
  let _: [String : () -> (Any, Any)] = exist[
    covariantAssocSubscriptComplex: { (_: Any.Type) in },
    { (_: Any.Type...) in },
    { (_: Array<Any>) in },
    { (_: Array<Array<Any>?>) in },
    { (_: () -> Any?) in }
  ]
}

// Contravariant references.
do {
  protocol P {
    associatedtype A

    func contravariantSelf1(_: Self?)
    func contravariantSelf2(_: () -> Self)
    func contravariantSelf3(_: Array<() -> Self>)
    func contravariantSelf4(_: [String : () -> Self])
    func contravariantSelf5(_: () -> (Self, Self))
    func contravariantSelf6(_: ((Self) -> Void) -> Void)
    func contravariantSelf7() -> (Self) -> Void
    func contravariantSelf8() -> Array<((Self) -> Void)?>
    func contravariantSelf9(_: [String : (() -> Self)?])
    func contravariantSelf10() -> (Array<[String : Self??]>) -> Void
    func contravariantSelf11(_: Self.Type)
    func contravariantAssoc1(_: A?)
    func contravariantAssoc2(_: () -> A)
    func contravariantAssoc3(_: Array<() -> A>)
    func contravariantAssoc4(_: [String : () -> A])
    func contravariantAssoc5(_: () -> (A, A))
    func contravariantAssoc6(_: ((A) -> Void) -> Void)
    func contravariantAssoc7() -> (A) -> Void
    func contravariantAssoc8() -> Array<((A) -> Void)?>
    func contravariantAssoc9(_: [String : (() -> A)?])
    func contravariantAssoc10() -> (Array<[String : A??]>) -> Void
    func contravariantAssoc11(_: A.Type)

    var contravariantSelfProp1: (Self?) -> Void { get }
    var contravariantSelfProp2: (() -> Self) -> Void { get }
    var contravariantSelfProp3: (Array<() -> Self>) -> Void { get }
    var contravariantSelfProp4: ([String : () -> Self]) -> Void { get }
    var contravariantSelfProp5: (() -> (Self, Self)) -> Void { get }
    var contravariantSelfProp6: (((Self) -> Void) -> Void) -> Void { get }
    var contravariantSelfProp7: (Self) -> Void { get }
    var contravariantSelfProp8: Array<((Self) -> Void)?> { get }
    var contravariantSelfProp9: ([String : (() -> Self)?]) -> Void { get }
    var contravariantSelfProp10: (Array<[String : Self??]>) -> Void { get }
    var contravariantSelfProp11: (Self.Type) -> Void { get }
    var contravariantAssocProp1: (A?) -> Void { get }
    var contravariantAssocProp2: (() -> A) -> Void { get }
    var contravariantAssocProp3: (Array<() -> A>) -> Void { get }
    var contravariantAssocProp4: ([String : () -> A]) -> Void { get }
    var contravariantAssocProp5: (() -> (A, A)) -> Void { get }
    var contravariantAssocProp6: (((A) -> Void) -> Void) -> Void { get }
    var contravariantAssocProp7: (A) -> Void { get }
    var contravariantAssocProp8: Array<((A) -> Void)?> { get }
    var contravariantAssocProp9: ([String : (() -> A)?]) -> Void { get }
    var contravariantAssocProp10: (Array<[String : A??]>) -> Void { get }
    var contravariantAssocProp11: (A.Type) -> Void { get }

    subscript(contravariantSelfSubscript1 _: Self?) -> Void { get }
    subscript(contravariantSelfSubscript2 _: () -> Self) -> Void { get }
    subscript(contravariantSelfSubscript3 _: Array<() -> Self>) -> Void { get }
    subscript(contravariantSelfSubscript4 _: [String : () -> Self]) -> Void { get }
    subscript(contravariantSelfSubscript5 _: () -> (Self, Self)) -> Void { get }
    subscript(contravariantSelfSubscript6 _: ((Self) -> Void) -> Void) -> Void { get }
    subscript(contravariantSelfSubscript7 _: Void) -> (Self) -> Void { get }
    subscript(contravariantSelfSubscript8 _: Void) -> Array<((Self) -> Void)?> { get }
    subscript(contravariantSelfSubscript9 _: [String : (() -> Self)?]) -> Void { get }
    subscript(contravariantSelfSubscript10 _: Void) -> (Array<[String : Self??]>) -> Void { get }
    subscript(contravariantSelfSubscript11 _: Self.Type) -> Void { get }
    subscript(contravariantAssocSubscript1 _: A?) -> Void { get }
    subscript(contravariantAssocSubscript2 _: () -> A) -> Void { get }
    subscript(contravariantAssocSubscript3 _: Array<() -> A>) -> Void { get }
    subscript(contravariantAssocSubscript4 _: [String : () -> A]) -> Void { get }
    subscript(contravariantAssocSubscript5 _: () -> (A, A)) -> Void { get }
    subscript(contravariantAssocSubscript6 _: ((A) -> Void) -> Void) -> Void { get }
    subscript(contravariantAssocSubscript7 _: Void) -> (A) -> Void { get }
    subscript(contravariantAssocSubscript8 _: Void) -> Array<((A) -> Void)?> { get }
    subscript(contravariantAssocSubscript9 _: [String : (() -> A)?]) -> Void { get }
    subscript(contravariantAssocSubscript10 _: Void) -> (Array<[String : A??]>) -> Void { get }
    subscript(contravariantAssocSubscript11 _: A.Type) -> Void { get }
  }

  let exist: any P

  exist.contravariantSelf1(0) // expected-error {{member 'contravariantSelf1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf2(0) // expected-error {{member 'contravariantSelf2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf3(0) // expected-error {{member 'contravariantSelf3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf4(0) // expected-error {{member 'contravariantSelf4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf5(0) // expected-error {{member 'contravariantSelf5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf6(0) // expected-error {{member 'contravariantSelf6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf7() // expected-error {{member 'contravariantSelf7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf8() // expected-error {{member 'contravariantSelf8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf9(0) // expected-error {{member 'contravariantSelf9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf10() // expected-error {{member 'contravariantSelf10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelf11(0) // expected-error {{member 'contravariantSelf11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc1(0) // expected-error {{member 'contravariantAssoc1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc2(0) // expected-error {{member 'contravariantAssoc2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc3(0) // expected-error {{member 'contravariantAssoc3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc4(0) // expected-error {{member 'contravariantAssoc4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc5(0) // expected-error {{member 'contravariantAssoc5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc6(0) // expected-error {{member 'contravariantAssoc6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc7() // expected-error {{member 'contravariantAssoc7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc8() // expected-error {{member 'contravariantAssoc8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc9(0) // expected-error {{member 'contravariantAssoc9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc10() // expected-error {{member 'contravariantAssoc10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssoc11(0) // expected-error {{member 'contravariantAssoc11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist.contravariantSelfProp1 // expected-error {{member 'contravariantSelfProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp2 // expected-error {{member 'contravariantSelfProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp3 // expected-error {{member 'contravariantSelfProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp4 // expected-error {{member 'contravariantSelfProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp5 // expected-error {{member 'contravariantSelfProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp6 // expected-error {{member 'contravariantSelfProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp7 // expected-error {{member 'contravariantSelfProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp8 // expected-error {{member 'contravariantSelfProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp9 // expected-error {{member 'contravariantSelfProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp10 // expected-error {{member 'contravariantSelfProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantSelfProp11 // expected-error {{member 'contravariantSelfProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp1 // expected-error {{member 'contravariantAssocProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp2 // expected-error {{member 'contravariantAssocProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp3 // expected-error {{member 'contravariantAssocProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp4 // expected-error {{member 'contravariantAssocProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp5 // expected-error {{member 'contravariantAssocProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp6 // expected-error {{member 'contravariantAssocProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp7 // expected-error {{member 'contravariantAssocProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp8 // expected-error {{member 'contravariantAssocProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp9 // expected-error {{member 'contravariantAssocProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp10 // expected-error {{member 'contravariantAssocProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantAssocProp11 // expected-error {{member 'contravariantAssocProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist[contravariantSelfSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript2: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript3: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript7: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript9: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript10: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSelfSubscript11: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript2: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript3: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript7: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript9: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript10: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantAssocSubscript11: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
}

// Invariant references.
do {
  protocol P {
    associatedtype A

    func invariantSelf1(_: inout Self)
    func invariantSelf2(_: (inout Self) -> Void)
    func invariantSelf3(_: inout Array<() -> Self>)
    func invariantSelf4(_: Struct<Self>)
    func invariantSelf5() -> Struct<Self>
    func invariantSelf6() -> Struct<Self>.Inner
    func invariantSelf7(_: (Struct<Self>) -> Void)
    func invariantSelf8(_: Struct<(Self) -> Void>)
    func invariantSelf9(_: Struct<() -> Self>)
    func invariantSelf10(_: any P & Class<Self>)
    func invariantSelf11() -> Struct<Self>.InnerGeneric<Void>
    // https://github.com/apple/swift/issues/61934
    func invariantSelf12() -> any Sequence<Self>
    // FIXME
    // expected-error@+1 {{non-protocol, non-class type 'Sequence<Self>' cannot be used within a protocol-constrained type}}
    func invariantSelf13() -> any P & Sequence<Self>
    func invariantSelf14() -> (any Sequence<Self>).Type
    func invariantAssoc1(_: inout A)
    func invariantAssoc2(_: (inout A) -> Void)
    func invariantAssoc3(_: inout Array<() -> A>)
    func invariantAssoc4(_: Struct<A>)
    func invariantAssoc5() -> Struct<A>
    func invariantAssoc6() -> Struct<A>.Inner
    func invariantAssoc7(_: (Struct<A>) -> Void)
    func invariantAssoc8(_: Struct<(A) -> Void>)
    func invariantAssoc9(_: Struct<() -> A>)
    func invariantAssoc10(_: any P & Class<A>)
    func invariantAssoc11() -> Struct<A>.InnerGeneric<Void>
    func invariantAssoc12() -> any Sequence<A>
    // FIXME
    // expected-error@+1 {{non-protocol, non-class type 'Sequence<Self.A>' cannot be used within a protocol-constrained type}}
    func invariantAssoc13() -> any P & Sequence<A>
    func invariantAssoc14() -> (any Sequence<A>).Type

    var invariantSelfProp1: (inout Self) -> Void { get }
    var invariantSelfProp2: ((inout Self) -> Void) -> Void { get }
    var invariantSelfProp3: (inout Array<() -> Self>) -> Void { get }
    var invariantSelfProp4: (Struct<Self>) -> Void { get }
    var invariantSelfProp5: Struct<Self> { get }
    var invariantSelfProp6: Struct<Self>.Inner { get }
    var invariantSelfProp7: ((Struct<Self>) -> Void) -> Void { get }
    var invariantSelfProp8: (Struct<(Self) -> Void>) -> Void { get }
    var invariantSelfProp9: (Struct<() -> Self>) -> Void { get }
    var invariantSelfProp10: (any P & Class<Self>) -> Void { get }
    var invariantSelfProp11: Struct<Self>.InnerGeneric<Void> { get }
    var invariantAssocProp1: (inout A) -> Void { get }
    var invariantAssocProp2: ((inout A) -> Void) -> Void { get }
    var invariantAssocProp3: (inout Array<() -> A>) -> Void { get }
    var invariantAssocProp4: (Struct<A>) -> Void { get }
    var invariantAssocProp5: Struct<A> { get }
    var invariantAssocProp6: Struct<A>.Inner { get }
    var invariantAssocProp7: ((Struct<A>) -> Void) { get }
    var invariantAssocProp8: (Struct<(A) -> Void>) { get }
    var invariantAssocProp9: (Struct<() -> A>) -> Void { get }
    var invariantAssocProp10: (any P & Class<A>) -> Void { get }
    var invariantAssocProp11: Struct<A>.InnerGeneric<Void> { get }

    subscript(invariantSelfSubscript1 _: Struct<Self>) -> Void { get }
    subscript(invariantSelfSubscript2 _: Void) -> Struct<Self> { get }
    subscript(invariantSelfSubscript3 _: Void) -> Struct<Self>.Inner { get }
    subscript(invariantSelfSubscript4 _: (Struct<Self>) -> Void) -> Void { get }
    subscript(invariantSelfSubscript5 _: Struct<(Self) -> Void>) -> Void { get }
    subscript(invariantSelfSubscript6 _: Struct<() -> Self>) -> Void { get }
    subscript(invariantSelfSubscript7 _: any P & Class<Self>) -> Void { get }
    subscript(invariantSelfSubscript8 _: Void) -> Struct<Self>.InnerGeneric<Void> { get }
    subscript(invariantAssocSubscript1 _: Struct<A>) -> Void { get }
    subscript(invariantAssocSubscript2 _: Void) -> Struct<A> { get }
    subscript(invariantAssocSubscript3 _: Void) -> Struct<A>.Inner { get }
    subscript(invariantAssocSubscript4 _: (Struct<A>) -> Void) -> Void { get }
    subscript(invariantAssocSubscript5 _: Struct<(A) -> Void>) -> Void { get }
    subscript(invariantAssocSubscript6 _: Struct<() -> A>) -> Void { get }
    subscript(invariantAssocSubscript7 _: any P & Class<A>) -> Void { get }
    subscript(invariantAssocSubscript8 _: Void) -> Struct<A>.InnerGeneric<Void> { get }
  }

  let exist: any P

  exist.invariantSelf1(0) // expected-error {{member 'invariantSelf1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf2(0) // expected-error {{member 'invariantSelf2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf3(0) // expected-error {{member 'invariantSelf3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf4(0) // expected-error {{member 'invariantSelf4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf5() // expected-error {{member 'invariantSelf5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf6() // expected-error {{member 'invariantSelf6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf7(0) // expected-error {{member 'invariantSelf7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf8(0) // expected-error {{member 'invariantSelf8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf9(0) // expected-error {{member 'invariantSelf9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf10(0) // expected-error {{member 'invariantSelf10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelf11() // expected-error {{member 'invariantSelf11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  do {
    let a = SwiftType<any Sequence>.IsStaticTypeOfExprAssertion(exist.invariantSelf12())
    a.assert()
  }
  do {
    let a = SwiftType<any P & Sequence>.IsStaticTypeOfExprAssertion(exist.invariantSelf13())
    a.assert()
  }
  do {
    // FIXME: Because (any P<X>).Type to (any P).Type upcast is not supported.
    // expected-error@+1 {{member 'invariantSelf14()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let a = SwiftType<(any Sequence).Type>.IsStaticTypeOfExprAssertion(exist.invariantSelf14())
    a.assert()
  }
  exist.invariantAssoc1(0) // expected-error {{member 'invariantAssoc1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc2(0) // expected-error {{member 'invariantAssoc2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc3(0) // expected-error {{member 'invariantAssoc3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc4(0) // expected-error {{member 'invariantAssoc4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc5() // expected-error {{member 'invariantAssoc5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc6() // expected-error {{member 'invariantAssoc6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc7(0) // expected-error {{member 'invariantAssoc7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc8(0) // expected-error {{member 'invariantAssoc8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc9(0) // expected-error {{member 'invariantAssoc9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc10(0) // expected-error {{member 'invariantAssoc10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssoc11() // expected-error {{member 'invariantAssoc11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  do {
    let a = SwiftType<any Sequence>.IsStaticTypeOfExprAssertion(exist.invariantAssoc12())
    a.assert()
  }
  do {
    let a = SwiftType<any P & Sequence>.IsStaticTypeOfExprAssertion(exist.invariantAssoc13())
    a.assert()
  }
  do {
    // FIXME: Because (any P<X>).Type to (any P).Type upcast is not supported.
    // expected-error@+1 {{member 'invariantAssoc14()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let a = SwiftType<(any Sequence).Type>.IsStaticTypeOfExprAssertion(exist.invariantAssoc14())
    a.assert()
  }

  exist.invariantSelfProp1 // expected-error {{member 'invariantSelfProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp2 // expected-error {{member 'invariantSelfProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp3 // expected-error {{member 'invariantSelfProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp4 // expected-error {{member 'invariantSelfProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp5 // expected-error {{member 'invariantSelfProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp6 // expected-error {{member 'invariantSelfProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp7 // expected-error {{member 'invariantSelfProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp8 // expected-error {{member 'invariantSelfProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp9 // expected-error {{member 'invariantSelfProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp10 // expected-error {{member 'invariantSelfProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantSelfProp11 // expected-error {{member 'invariantSelfProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp1 // expected-error {{member 'invariantAssocProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp2 // expected-error {{member 'invariantAssocProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp3 // expected-error {{member 'invariantAssocProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp4 // expected-error {{member 'invariantAssocProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp5 // expected-error {{member 'invariantAssocProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp6 // expected-error {{member 'invariantAssocProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp7 // expected-error {{member 'invariantAssocProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp8 // expected-error {{member 'invariantAssocProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp9 // expected-error {{member 'invariantAssocProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp10 // expected-error {{member 'invariantAssocProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantAssocProp11 // expected-error {{member 'invariantAssocProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist[invariantSelfSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript2: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript3: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript7: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSelfSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript2: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript3: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript7: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantAssocSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
}

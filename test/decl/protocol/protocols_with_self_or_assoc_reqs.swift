// RUN: %target-typecheck-verify-swift -disable-availability-checking

//===----------------------------------------------------------------------===//
// Use of protocols with Self or associated type requirements
//===----------------------------------------------------------------------===//

struct G<T> {
  class Inner {}
  struct InnerG<T> {}
}
class C<T> {}

protocol P1 {
  associatedtype Q

  // Methods
  func covariantSelfSimple() -> Self
  func covariantSelfComplex(_: (Self) -> Void,
                            _: (Self?) -> Void,
                            _: (Array<Self>) -> Void,
                            _: (Array<Array<Self>?>) -> Void
  ) -> [String : () -> (Self, Self)]
  func covariantAssocSimple() -> Q
  func covariantAssocComplex(_: (Q) -> Void,
                             _: (Q?) -> Void,
                             _: (Array<Q>) -> Void,
                             _: (Array<Array<Q>?>) -> Void
  ) -> [String : () -> (Q, Q)]

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
  func contravariantAssoc1(_: Q?)
  func contravariantAssoc2(_: () -> Q)
  func contravariantAssoc3(_: Array<() -> Q>)
  func contravariantAssoc4(_: [String : () -> Q])
  func contravariantAssoc5(_: () -> (Q, Q))
  func contravariantAssoc6(_: ((Q) -> Void) -> Void)
  func contravariantAssoc7() -> (Q) -> Void
  func contravariantAssoc8() -> Array<((Q) -> Void)?>
  func contravariantAssoc9(_: [String : (() -> Q)?])
  func contravariantAssoc10() -> (Array<[String : Q??]>) -> Void

  func invariantSelf1(_: inout Self)
  func invariantSelf2(_: (inout Self) -> Void)
  func invariantSelf3(_: inout Array<() -> Self>)
  func invariantSelf4(_: G<Self>)
  func invariantSelf5() -> G<Self>
  func invariantSelf6() -> G<Self>.Inner
  func invariantSelf7(_: (G<Self>) -> Void)
  func invariantSelf8(_: G<(Self) -> Void>)
  func invariantSelf9(_: G<() -> Self>)
  func invariantSelf10(_: P1 & C<Self>)
  func invariantSelf11() -> G<Self>.InnerG<Void>
  func invariantAssoc1(_: inout Q)
  func invariantAssoc2(_: (inout Q) -> Void)
  func invariantAssoc3(_: inout Array<() -> Q>)
  func invariantAssoc4(_: G<Q>)
  func invariantAssoc5() -> G<Q>
  func invariantAssoc6() -> G<Q>.Inner
  func invariantAssoc7(_: (G<Q>) -> Void)
  func invariantAssoc8(_: G<(Q) -> Void>)
  func invariantAssoc9(_: G<() -> Q>)
  func invariantAssoc10(_: P1 & C<Q>)
  func invariantAssoc11() -> G<Q>.InnerG<Void>

  // Properties
  var covariantSelfPropSimple: Self { get }
  var covariantSelfPropComplex: (
    _: (Self) -> Void,
    _: (Self?) -> Void,
    _: (Array<Self>) -> Void,
    _: (Array<Array<Self>?>) -> Void
  ) -> [String : () -> (Self, Self)] { get }
  var covariantAssocPropSimple: Q { get }
  var covariantAssocPropComplex: (
    _: (Q) -> Void,
    _: (Q?) -> Void,
    _: (Array<Q>) -> Void,
    _: (Array<Array<Q>?>) -> Void
  ) -> [String : () -> (Q, Q)] { get }

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
  var contravariantAssocProp1: (Q?) -> Void { get }
  var contravariantAssocProp2: (() -> Q) -> Void { get }
  var contravariantAssocProp3: (Array<() -> Q>) -> Void { get }
  var contravariantAssocProp4: ([String : () -> Q]) -> Void { get }
  var contravariantAssocProp5: (() -> (Q, Q)) -> Void { get }
  var contravariantAssocProp6: (((Q) -> Void) -> Void) -> Void { get }
  var contravariantAssocProp7: (Q) -> Void { get }
  var contravariantAssocProp8: Array<((Q) -> Void)?> { get }
  var contravariantAssocProp9: ([String : (() -> Q)?]) -> Void { get }
  var contravariantAssocProp10: (Array<[String : Q??]>) -> Void { get }

  var invariantSelfProp1: (inout Self) -> Void { get }
  var invariantSelfProp2: ((inout Self) -> Void) -> Void { get }
  var invariantSelfProp3: (inout Array<() -> Self>) -> Void { get }
  var invariantSelfProp4: (G<Self>) -> Void { get }
  var invariantSelfProp5: G<Self> { get }
  var invariantSelfProp6: G<Self>.Inner { get }
  var invariantSelfProp7: ((G<Self>) -> Void) -> Void { get }
  var invariantSelfProp8: (G<(Self) -> Void>) -> Void { get }
  var invariantSelfProp9: (G<() -> Self>) -> Void { get }
  var invariantSelfProp10: (P1 & C<Self>) -> Void { get }
  var invariantSelfProp11: G<Self>.InnerG<Void> { get }
  var invariantAssocProp1: (inout Q) -> Void { get }
  var invariantAssocProp2: ((inout Q) -> Void) -> Void { get }
  var invariantAssocProp3: (inout Array<() -> Q>) -> Void { get }
  var invariantAssocProp4: (G<Q>) -> Void { get }
  var invariantAssocProp5: G<Q> { get }
  var invariantAssocProp6: G<Q>.Inner { get }
  var invariantAssocProp7: ((G<Q>) -> Void) { get }
  var invariantAssocProp8: (G<(Q) -> Void>) { get }
  var invariantAssocProp9: (G<() -> Q>) -> Void { get }
  var invariantAssocProp10: (P1 & C<Q>) -> Void { get }
  var invariantAssocProp11: G<Q>.InnerG<Void> { get }

  // Subscripts
  subscript(covariantSelfSubscriptSimple _: Void) -> Self { get }
  subscript(covariantSelfSubscriptComplex _: (Self) -> Void,
            _: (Self?) -> Void,
            _: (Array<Self>) -> Void,
            _: (Array<Array<Self>?>) -> Void
  ) -> [String : () -> (Self, Self)] { get }
  subscript(covariantAssocSubscriptSimple _: Void) -> Q { get }
  subscript(covariantAssocSubscriptComplex _: (Q) -> Void,
            _: (Q?) -> Void,
            _: (Array<Q>) -> Void,
            _: (Array<Array<Q>?>) -> Void
  ) -> [String : () -> (Q, Q)] { get }

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
  subscript(contravariantAssocSubscript1 _: Q?) -> Void { get }
  subscript(contravariantAssocSubscript2 _: () -> Q) -> Void { get }
  subscript(contravariantAssocSubscript3 _: Array<() -> Q>) -> Void { get }
  subscript(contravariantAssocSubscript4 _: [String : () -> Q]) -> Void { get }
  subscript(contravariantAssocSubscript5 _: () -> (Q, Q)) -> Void { get }
  subscript(contravariantAssocSubscript6 _: ((Q) -> Void) -> Void) -> Void { get }
  subscript(contravariantAssocSubscript7 _: Void) -> (Q) -> Void { get }
  subscript(contravariantAssocSubscript8 _: Void) -> Array<((Q) -> Void)?> { get }
  subscript(contravariantAssocSubscript9 _: [String : (() -> Q)?]) -> Void { get }
  subscript(contravariantAssocSubscript10 _: Void) -> (Array<[String : Q??]>) -> Void { get }

  subscript(invariantSelfSubscript1 _: G<Self>) -> Void { get }
  subscript(invariantSelfSubscript2 _: Void) -> G<Self> { get }
  subscript(invariantSelfSubscript3 _: Void) -> G<Self>.Inner { get }
  subscript(invariantSelfSubscript4 _: (G<Self>) -> Void) -> Void { get }
  subscript(invariantSelfSubscript5 _: G<(Self) -> Void>) -> Void { get }
  subscript(invariantSelfSubscript6 _: G<() -> Self>) -> Void { get }
  subscript(invariantSelfSubscript7 _: P1 & C<Self>) -> Void { get }
  subscript(invariantSelfSubscript8 _: Void) -> G<Self>.InnerG<Void> { get }
  subscript(invariantAssocSubscript1 _: G<Q>) -> Void { get }
  subscript(invariantAssocSubscript2 _: Void) -> G<Q> { get }
  subscript(invariantAssocSubscript3 _: Void) -> G<Q>.Inner { get }
  subscript(invariantAssocSubscript4 _: (G<Q>) -> Void) -> Void { get }
  subscript(invariantAssocSubscript5 _: G<(Q) -> Void>) -> Void { get }
  subscript(invariantAssocSubscript6 _: G<() -> Q>) -> Void { get }
  subscript(invariantAssocSubscript7 _: P1 & C<Q>) -> Void { get }
  subscript(invariantAssocSubscript8 _: Void) -> G<Q>.InnerG<Void> { get }
}
@available(macOS 10.15, *)
extension P1 {
  func invariantSelf1_1() -> some P1 { self }
  var invariantSelfProp1_1: some P1 { self }
  subscript(invariantSelfSubscript1_1: Void) -> some P1 { self }
}

do {
  func testP1(arg: P1) {
    _ = arg.covariantSelfSimple() // ok
    let _: P1 = arg.covariantSelfSimple() // ok
    _ = arg.covariantSelfComplex({ _ in },  { _ in }, { _ in }, { _ in }) // ok
    let _: [String : () -> (P1, P1)] = arg.covariantSelfComplex(
      { (x: P1) in },
      { (x: P1?) in },
      { (x: Array<P1>) in },
      { (x: Array<Array<P1>?>) in }
    ) // ok
    arg.covariantAssocSimple // expected-error {{member 'covariantAssocSimple' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.covariantAssocComplex({ _ in }, { _ in }, { _ in }, { _ in }) // expected-error {{member 'covariantAssocComplex' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: expected-error@-1 {{unable to infer type of a closure parameter '_' in the current context}}

    _ = arg.covariantSelfPropSimple // ok
    let _: P1 = arg.covariantSelfPropSimple // ok
    _ = arg.covariantSelfPropComplex // ok
    let _: (
      _: (P1) -> Void,
      _: (P1?) -> Void,
      _: (Array<P1>) -> Void,
      _: (Array<Array<P1>?>) -> Void
    ) -> [String : () -> (P1, P1)] = arg.covariantSelfPropComplex // ok
    arg.covariantAssocPropSimple // expected-error {{member 'covariantAssocPropSimple' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.covariantAssocPropComplex // expected-error {{member 'covariantAssocPropComplex' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    _ = arg[covariantSelfSubscriptSimple: ()] // ok
    let _: P1 = arg[covariantSelfSubscriptSimple: ()] // ok
    _ = arg[covariantSelfSubscriptComplex: { _ in },  { _ in }, { _ in }, { _ in }] // ok
    let _: [String : () -> (P1, P1)] = arg[
      covariantSelfSubscriptComplex: { (x: P1) in },
      { (x: P1?) in },
      { (x: Array<P1>) in },
      { (x: Array<Array<P1>?>) in }
    ] // ok
    arg[covariantAssocSubscriptSimple: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[covariantAssocSubscriptComplex: { _ in }, { _ in }, { _ in }, { _ in }] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    arg.contravariantSelf1(0) // expected-error {{member 'contravariantSelf1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{argument type 'Int' does not conform to expected type 'P1'}}
    arg.contravariantSelf2(0) // expected-error {{member 'contravariantSelf2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> P1'}}
    arg.contravariantSelf3(0) // expected-error {{member 'contravariantSelf3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1>'}}
    arg.contravariantSelf4(0) // expected-error {{member 'contravariantSelf4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : () -> P1]'}}
    arg.contravariantSelf5(0) // expected-error {{member 'contravariantSelf5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> (P1, P1)'}}
    arg.contravariantSelf6(0) // expected-error {{member 'contravariantSelf6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '((P1) -> Void) -> Void'}}
    arg.contravariantSelf7() // expected-error {{member 'contravariantSelf7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelf8() // expected-error {{member 'contravariantSelf8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelf9(0) // expected-error {{member 'contravariantSelf9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : (() -> P1)?]'}}
    arg.contravariantSelf10() // expected-error {{member 'contravariantSelf10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssoc1(0) // expected-error {{member 'contravariantAssoc1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{cannot convert value of type 'Int' to expected argument type 'P1.Q?'}}
    arg.contravariantAssoc2(0) // expected-error {{member 'contravariantAssoc2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> P1.Q'}}
    arg.contravariantAssoc3(0) // expected-error {{member 'contravariantAssoc3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1.Q>'}}
    arg.contravariantAssoc4(0) // expected-error {{member 'contravariantAssoc4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : () -> P1.Q]'}}
    arg.contravariantAssoc5(0) // expected-error {{member 'contravariantAssoc5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> (P1.Q, P1.Q)'}}
    arg.contravariantAssoc6(0) // expected-error {{member 'contravariantAssoc6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '((P1.Q) -> Void) -> Void'}}
    arg.contravariantAssoc7() // expected-error {{member 'contravariantAssoc7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssoc8() // expected-error {{member 'contravariantAssoc8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssoc9(0) // expected-error {{member 'contravariantAssoc9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : (() -> P1.Q)?]'}}
    arg.contravariantAssoc10() // expected-error {{member 'contravariantAssoc10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}


    arg.invariantSelf1(0) // expected-error {{member 'invariantSelf1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{argument type 'Int' does not conform to expected type 'P1'}}
    if #available(macOS 10.15, *) {
      arg.invariantSelf1_1() // expected-error {{member 'invariantSelf1_1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    }
    arg.invariantSelf2(0) // expected-error {{member 'invariantSelf2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(inout P1) -> Void'}}
    arg.invariantSelf3(0) // expected-error {{member 'invariantSelf3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1>'}}
    arg.invariantSelf4(0) // expected-error {{member 'invariantSelf4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<P1>'}}
    arg.invariantSelf5() // expected-error {{member 'invariantSelf5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelf6() // expected-error {{member 'invariantSelf6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelf7(0) // expected-error {{member 'invariantSelf7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(G<P1>) -> Void'}}
    arg.invariantSelf8(0) // expected-error {{member 'invariantSelf8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<(P1) -> Void>'}}
    arg.invariantSelf9(0) // expected-error {{member 'invariantSelf9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<() -> P1>'}}
    arg.invariantSelf10(0) // expected-error {{member 'invariantSelf10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'C<P1>'}}
    arg.invariantSelf11() // expected-error {{member 'invariantSelf11' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssoc1(0) // expected-error {{member 'invariantAssoc1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{cannot convert value of type 'Int' to expected argument type 'P1.Q'}}
    arg.invariantAssoc2(0) // expected-error {{member 'invariantAssoc2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(inout P1.Q) -> Void'}}
    arg.invariantAssoc3(0) // expected-error {{member 'invariantAssoc3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1.Q>'}}
    arg.invariantAssoc4(0) // expected-error {{member 'invariantAssoc4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<P1.Q>'}}
    arg.invariantAssoc5() // expected-error {{member 'invariantAssoc5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssoc6() // expected-error {{member 'invariantAssoc6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssoc7(0) // expected-error {{member 'invariantAssoc7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(G<P1.Q>) -> Void'}}
    arg.invariantAssoc8(0) // expected-error {{member 'invariantAssoc8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<(P1.Q) -> Void>'}}
    arg.invariantAssoc9(0) // expected-error {{member 'invariantAssoc9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<() -> P1.Q>'}}
    arg.invariantAssoc10(0) // expected-error {{member 'invariantAssoc10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'C<P1.Q>'}}
    arg.invariantAssoc11() // expected-error {{member 'invariantAssoc11' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    arg.contravariantSelfProp1 // expected-error {{member 'contravariantSelfProp1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp2 // expected-error {{member 'contravariantSelfProp2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp3 // expected-error {{member 'contravariantSelfProp3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp4 // expected-error {{member 'contravariantSelfProp4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp5 // expected-error {{member 'contravariantSelfProp5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp6 // expected-error {{member 'contravariantSelfProp6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp7 // expected-error {{member 'contravariantSelfProp7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp8 // expected-error {{member 'contravariantSelfProp8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp9 // expected-error {{member 'contravariantSelfProp9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantSelfProp10 // expected-error {{member 'contravariantSelfProp10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp1 // expected-error {{member 'contravariantAssocProp1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp2 // expected-error {{member 'contravariantAssocProp2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp3 // expected-error {{member 'contravariantAssocProp3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp4 // expected-error {{member 'contravariantAssocProp4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp5 // expected-error {{member 'contravariantAssocProp5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp6 // expected-error {{member 'contravariantAssocProp6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp7 // expected-error {{member 'contravariantAssocProp7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp8 // expected-error {{member 'contravariantAssocProp8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp9 // expected-error {{member 'contravariantAssocProp9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.contravariantAssocProp10 // expected-error {{member 'contravariantAssocProp10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    arg.invariantSelfProp1 // expected-error {{member 'invariantSelfProp1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    if #available(macOS 10.15, *) {
      arg.invariantSelfProp1_1 // expected-error {{member 'invariantSelfProp1_1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    }
    arg.invariantSelfProp2 // expected-error {{member 'invariantSelfProp2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp3 // expected-error {{member 'invariantSelfProp3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp4 // expected-error {{member 'invariantSelfProp4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp5 // expected-error {{member 'invariantSelfProp5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp6 // expected-error {{member 'invariantSelfProp6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp7 // expected-error {{member 'invariantSelfProp7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp8 // expected-error {{member 'invariantSelfProp8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp9 // expected-error {{member 'invariantSelfProp9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp10 // expected-error {{member 'invariantSelfProp10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantSelfProp11 // expected-error {{member 'invariantSelfProp11' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp1 // expected-error {{member 'invariantAssocProp1' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp2 // expected-error {{member 'invariantAssocProp2' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp3 // expected-error {{member 'invariantAssocProp3' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp4 // expected-error {{member 'invariantAssocProp4' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp5 // expected-error {{member 'invariantAssocProp5' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp6 // expected-error {{member 'invariantAssocProp6' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp7 // expected-error {{member 'invariantAssocProp7' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp8 // expected-error {{member 'invariantAssocProp8' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp9 // expected-error {{member 'invariantAssocProp9' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp10 // expected-error {{member 'invariantAssocProp10' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg.invariantAssocProp11 // expected-error {{member 'invariantAssocProp11' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    arg[contravariantSelfSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{argument type 'Int' does not conform to expected type 'P1'}}
    arg[contravariantSelfSubscript2: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> P1'}}
    arg[contravariantSelfSubscript3: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1>'}}
    arg[contravariantSelfSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : () -> P1]'}}
    arg[contravariantSelfSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> (P1, P1)'}}
    arg[contravariantSelfSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '((P1) -> Void) -> Void'}}
    arg[contravariantSelfSubscript7: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[contravariantSelfSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[contravariantSelfSubscript9: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : (() -> P1)?]'}}
    arg[contravariantSelfSubscript10: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[contravariantAssocSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{cannot convert value of type 'Int' to expected argument type 'P1.Q?'}}
    arg[contravariantAssocSubscript2: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> P1.Q'}}
    arg[contravariantAssocSubscript3: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'Array<() -> P1.Q>'}}
    arg[contravariantAssocSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : () -> P1.Q]'}}
    arg[contravariantAssocSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> (P1.Q, P1.Q)'}}
    arg[contravariantAssocSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '((P1.Q) -> Void) -> Void'}}
    arg[contravariantAssocSubscript7: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[contravariantAssocSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[contravariantAssocSubscript9: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '[String : (() -> P1.Q)?]'}}
    arg[contravariantAssocSubscript10: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}

    arg[invariantSelfSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{cannot convert value of type 'Int' to expected argument type 'G<P1>'}}
    arg[invariantSelfSubscript2: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[invariantSelfSubscript3: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[invariantSelfSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(G<P1>) -> Void'}}
    arg[invariantSelfSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<(P1) -> Void>'}}
    arg[invariantSelfSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<() -> P1>'}}
    arg[invariantSelfSubscript7: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'C<P1>'}}
    arg[invariantSelfSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[invariantAssocSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // FIXME: Silence these since we cannot make use of the member anyway.
    // expected-error@-2 {{cannot convert value of type 'Int' to expected argument type 'G<P1.Q>'}}
    arg[invariantAssocSubscript2: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[invariantAssocSubscript3: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    arg[invariantAssocSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(G<P1.Q>) -> Void'}}
    arg[invariantAssocSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<(P1.Q) -> Void>'}}
    arg[invariantAssocSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'G<() -> P1.Q>'}}
    arg[invariantAssocSubscript7: 0] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'C<P1.Q>'}}
    arg[invariantAssocSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1'; use a generic constraint instead}}
  }
}

protocol P1_TypeMemberOnInstanceAndViceVersa {
  static func static_covariantSelfMethod() -> Self
  static var static_covariantSelfProp: Self { get }
  static subscript(static_covariantSelfSubscript _: Void) -> Self { get }

  static func static_invariantSelfMethod() -> G<Self>
  static var static_invariantSelfProp: G<Self> { get }
  static subscript(static_invariantSelfSubscript _: Void) -> G<Self> { get }

  func covariantSelfMethod() -> Self

  func invariantSelfMethod() -> G<Self>
  var invariantSelfProp: G<Self> { get }
  subscript(invariantSelfSubscript _: Void) -> G<Self> { get }
}
do {
  // Test that invalid reference errors prevail over unsupported existential
  // member accesses.
  func test(protoMeta: P1_TypeMemberOnInstanceAndViceVersa.Protocol,
            existMeta: P1_TypeMemberOnInstanceAndViceVersa.Type,
            instance: P1_TypeMemberOnInstanceAndViceVersa) {
    // P1_TypeMemberOnInstanceAndViceVersa.Protocol
    protoMeta.static_invariantSelfMethod() // expected-error {{static member 'static_invariantSelfMethod' cannot be used on protocol metatype 'P1_TypeMemberOnInstanceAndViceVersa.Protocol'}}
    protoMeta.static_invariantSelfProp // expected-error {{static member 'static_invariantSelfProp' cannot be used on protocol metatype 'P1_TypeMemberOnInstanceAndViceVersa.Protocol'}}
    protoMeta[static_invariantSelfSubscript: ()] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P1_TypeMemberOnInstanceAndViceVersa.Protocol'}}
    _ = protoMeta.covariantSelfMethod // ok
    protoMeta.invariantSelfMethod // expected-error {{member 'invariantSelfMethod' cannot be used on value of protocol type 'P1_TypeMemberOnInstanceAndViceVersa.Protocol'; use a generic constraint instead}}
    protoMeta.invariantSelfProp // expected-error {{instance member 'invariantSelfProp' cannot be used on type 'P1_TypeMemberOnInstanceAndViceVersa'}}
    protoMeta[invariantSelfSubscript: ()] // expected-error {{instance member 'subscript' cannot be used on type 'P1_TypeMemberOnInstanceAndViceVersa'}}

    // P1_TypeMemberOnInstanceAndViceVersa.Type
    _ = existMeta.static_covariantSelfMethod // ok
    _ = existMeta.static_covariantSelfProp // ok
    _ = existMeta[static_covariantSelfSubscript: ()] // ok
    existMeta.static_invariantSelfMethod // expected-error {{member 'static_invariantSelfMethod' cannot be used on value of protocol type 'P1_TypeMemberOnInstanceAndViceVersa.Type'; use a generic constraint instead}}
    existMeta.static_invariantSelfProp // expected-error {{member 'static_invariantSelfProp' cannot be used on value of protocol type 'P1_TypeMemberOnInstanceAndViceVersa.Type'; use a generic constraint instead}}
    existMeta[static_invariantSelfSubscript: ()] // expected-error {{member 'subscript' cannot be used on value of protocol type 'P1_TypeMemberOnInstanceAndViceVersa.Type'; use a generic constraint instead}}
    existMeta.invariantSelfMethod // expected-error {{instance member 'invariantSelfMethod' cannot be used on type 'P1_TypeMemberOnInstanceAndViceVersa'}}
    existMeta.invariantSelfProp // expected-error {{instance member 'invariantSelfProp' cannot be used on type 'P1_TypeMemberOnInstanceAndViceVersa'}}
    existMeta[invariantSelfSubscript: ()] // expected-error {{instance member 'subscript' cannot be used on type 'P1_TypeMemberOnInstanceAndViceVersa'}}

    // P1_TypeMemberOnInstanceAndViceVersa
    instance.static_invariantSelfMethod // expected-error {{static member 'static_invariantSelfMethod' cannot be used on instance of type 'P1_TypeMemberOnInstanceAndViceVersa'}}
    instance.static_invariantSelfProp // expected-error {{static member 'static_invariantSelfProp' cannot be used on instance of type 'P1_TypeMemberOnInstanceAndViceVersa'}}
    instance[static_invariantSelfSubscript: ()] // expected-error {{static member 'subscript' cannot be used on instance of type 'P1_TypeMemberOnInstanceAndViceVersa'}}
  }
}

// Settable storage members with a 'Self' result type may not be used with an
// existential base.
protocol P2 {
  subscript() -> Self { get set }

  var prop: Self { get set }
}
func takesP2(p2: P2) {
  _ = p2[]
  // expected-error@-1{{member 'subscript' cannot be used on value of protocol type 'P2'; use a generic constraint instead}}
  _ = p2.prop
  // expected-error@-1{{member 'prop' cannot be used on value of protocol type 'P2'; use a generic constraint instead}}
}

protocol MiscTestsProto {
  associatedtype Assoc
  func runce<A>(_: A)
  func spoon(_: Self)

  associatedtype R : IteratorProtocol, Sequence
  func getR() -> R

  subscript(intToAssoc _: Int) -> Assoc { get }
  subscript(intToInt _: Int) -> Int { get }
}
do {
  func miscTests(_ arg: MiscTestsProto) { // ok
    arg.runce(5)

    do {
      // FIXME: Crummy diagnostics.
      var x = arg.getR() // expected-error{{member 'getR' cannot be used on value of protocol type 'MiscTestsProto'; use a generic constraint instead}}
      x.makeIterator()
      x.next()
      x.nonexistent()
    }

    var _: Int = arg[intToInt: 17]
    _ = arg[intToAssoc: 17] // expected-error{{member 'subscript' cannot be used on value of protocol type 'MiscTestsProto'; use a generic constraint instead}}
  }

  func existentialSequence(_ e: Sequence) {
    var x = e.makeIterator() // expected-error{{member 'makeIterator' cannot be used on value of protocol type 'Sequence'; use a generic constraint instead}}
    x.next()
    x.nonexistent()
  }
}

// RUN: %empty-directory(%t)
// RUN: %gyb -D SELF=1 %s -o %t/self.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.9-abi-triple %t/self.swift
// RUN: %gyb -D SELF=0 %s -o %t/associatedtype.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.9-abi-triple %t/associatedtype.swift

%{
  is_self_case = int(SELF) != 0
  type = "Self" if is_self_case else "Self.A"
  upper_bound = "any P" if is_self_case else "Any"
}%

struct Struct<T> {
  class Inner {}
  struct InnerGeneric<U> {}
}
class Class<T> {}

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

// Covariant references.
do {
  protocol P {
    associatedtype A

    func covariant1() -> ${type}
    func covariant2() -> ${type}?
    func covariant3() -> ${type}.Type
    func covariant4() -> (${type}, ${type})
    func covariant5() -> Array<${type}>
    func covariant6() -> [String : ${type}]
    func covariant7(_: (${type}) -> Void)
    func covariant8(_: (${type}...) -> Void)
    func covariant9() -> ${type}?.Type
    func covariantComplex(
      _: (${type}.Type) -> Void,
      _: (${type}.Type...) -> Void,
      _: (Array<${type}>) -> Void,
      _: (Array<Array<${type}>?>) -> Void,
      _: (@escaping () -> ${type}?) -> Void
    ) -> [String : () -> (${type}, ${type})]

    var covariantProp1: ${type} { get }
    var covariantProp2: ${type}? { get }
    var covariantProp3: ${type}.Type { get }
    var covariantProp4: (${type}, ${type}) { get }
    var covariantProp5: Array<${type}> { get }
    var covariantProp6: [String : ${type}] { get }
    var covariantProp7: ((${type}) -> Void) -> Void { get }
    var covariantProp8: ((${type}...) -> Void) -> Void { get }
    var covariantPropComplex: (
      (${type}.Type) -> Void,
      (${type}.Type...) -> Void,
      (Array<${type}>) -> Void,
      (Array<Array<${type}>?>) -> Void,
      (() -> ${type}?) -> Void
    ) -> [String : () -> (${type}, ${type})] { get }

    subscript(covariantSubscript1 _: Void) -> ${type} { get }
    subscript(covariantSubscript2 _: Void) -> ${type}? { get }
    subscript(covariantSubscript3 _: Void) -> ${type}.Type { get }
    subscript(covariantSubscript4 _: Void) -> (${type}, ${type}) { get }
    subscript(covariantSubscript5 _: Void) -> Array<${type}> { get }
    subscript(covariantSubscript6 _: Void) -> [String : ${type}] { get }
    subscript(covariantSubscript7 _: (${type}) -> Void) -> ${type} { get }
    subscript(covariantSubscript8 _: (${type}...) -> Void) -> Self { get }
    subscript(
      covariantSubscriptComplex _: (${type}.Type) -> Void,
      _: (${type}.Type...) -> Void,
      _: (Array<${type}>) -> Void,
      _: (Array<Array<${type}>?>) -> Void,
      _: (@escaping () -> ${type}?) -> Void
    ) -> [String : () -> (${type}, ${type})] { get }
  }

  let exist: any P

  do {
    do {
      var types = SwiftTypePair(typeOf: exist.covariant1(), type2: SwiftType<${upper_bound}>.self)
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(typeOf: exist.covariant1, type2: SwiftType<() -> ${upper_bound}>.self)
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      var types = SwiftTypePair(typeOf: exist.covariant2(), type2: SwiftType<(${upper_bound})?>.self)
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(typeOf: exist.covariant2, type2: SwiftType<() -> (${upper_bound})?>.self)
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      var types = SwiftTypePair(typeOf: exist.covariant3(), type2: SwiftType<${upper_bound}.Type>.self)
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(typeOf: exist.covariant3, type2: SwiftType<() -> ${upper_bound}.Type>.self)
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant4(),
        type2: SwiftType<(${upper_bound}, ${upper_bound})>.self
      )
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant4,
        type2: SwiftType<() -> (${upper_bound}, ${upper_bound})>.self
      )
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant5(),
        type2: SwiftType<Array<${upper_bound}>>.self
      )
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant5,
        type2: SwiftType<() -> Array<${upper_bound}>>.self
      )
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant6(),
        type2: SwiftType<[String : ${upper_bound}]>.self
      )
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant6,
        type2: SwiftType<() -> [String : ${upper_bound}]>.self
      )
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      exist.covariant7 { x in
        var types = SwiftTypePair(typeOf: x, type2: SwiftType<${upper_bound}>.self)
        types.assertTypesAreEqual()
      }
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant7,
        type2: SwiftType<((${upper_bound}) -> Void) -> Void>.self
      )
      types.assertTypesAreEqual()
    }
  }
  do {
    do {
      exist.covariant8 { x in
        var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}>>.self)
        types.assertTypesAreEqual()
      }
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariant8,
        type2: SwiftType<((${upper_bound}...) -> Void) -> Void>.self
      )
      types.assertTypesAreEqual()
    }
  }
  do {
    // FIXME: Because B?.Type to A?.Type upcast is not supported.
    var types = SwiftTypePair(
      // expected-error@+1 {{member 'covariant9()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
      typeOf: exist.covariant9(),
      type2: SwiftType<(${upper_bound})?.Type>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariantComplex(
          { x in
            var types = SwiftTypePair(typeOf: x, type2: SwiftType<${upper_bound}.Type>.self)
            types.assertTypesAreEqual()
          },
          { x in
            var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}.Type>>.self)
            types.assertTypesAreEqual()
          },
          { x in
            var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}>>.self)
            types.assertTypesAreEqual()
          },
          { x in
            var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<Array<${upper_bound}>?>>.self)
            types.assertTypesAreEqual()
          },
          { x in
            var types = SwiftTypePair(typeOf: x, type2: SwiftType<() -> (${upper_bound})?>.self)
            types.assertTypesAreEqual()
          }
        ),
        type2: SwiftType<[String : () -> (${upper_bound}, ${upper_bound})]>.self
      )
      types.assertTypesAreEqual()
    }
    do {
      var types = SwiftTypePair(
        typeOf: exist.covariantComplex,
        type2: SwiftType<
          (
            (${upper_bound}.Type) -> Void,
            (${upper_bound}.Type...) -> Void,
            (Array<${upper_bound}>) -> Void,
            (Array<Array<${upper_bound}>?>) -> Void,
            (@escaping () -> (${upper_bound})?) -> Void
          ) -> [String : () -> (${upper_bound}, ${upper_bound})]
        >.self
      )
      types.assertTypesAreEqual()
    }
  }

  do {
    var types = SwiftTypePair(typeOf: exist.covariantProp1, type2: SwiftType<${upper_bound}>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.covariantProp2, type2: SwiftType<(${upper_bound})?>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.covariantProp3, type2: SwiftType<${upper_bound}.Type>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantProp4,
      type2: SwiftType<(${upper_bound}, ${upper_bound})>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantProp5,
      type2: SwiftType<Array<${upper_bound}>>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantProp6,
      type2: SwiftType<[String : ${upper_bound}]>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantProp7,
      type2: SwiftType<((${upper_bound}) -> Void) -> Void>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantProp8,
      type2: SwiftType<((${upper_bound}...) -> Void) -> Void>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist.covariantPropComplex,
      type2: SwiftType<
        (
          (${upper_bound}.Type) -> Void,
          (${upper_bound}.Type...) -> Void,
          (Array<${upper_bound}>) -> Void,
          (Array<Array<${upper_bound}>?>) -> Void,
          (() -> (${upper_bound})?) -> Void
        ) -> [String : () -> (${upper_bound}, ${upper_bound})]
      >.self
    )
    types.assertTypesAreEqual()
  }

  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript1: ()],
      type2: SwiftType<${upper_bound}>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript2: ()],
      type2: SwiftType<(${upper_bound})?>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript3: ()],
      type2: SwiftType<${upper_bound}.Type>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript4: ()],
      type2: SwiftType<(${upper_bound}, ${upper_bound})>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript5: ()],
      type2: SwiftType<Array<${upper_bound}>>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[covariantSubscript6: ()],
      type2: SwiftType<[String : ${upper_bound}]>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[
        covariantSubscript7: { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<${upper_bound}>.self)
          types.assertTypesAreEqual()
        }
      ],
      type2: SwiftType<${upper_bound}>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[
        covariantSubscript8: { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}>>.self)
          types.assertTypesAreEqual()
        }
      ],
      type2: SwiftType<any P>.self
    )
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(
      typeOf: exist[
        covariantSubscriptComplex: { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<${upper_bound}.Type>.self)
          types.assertTypesAreEqual()
        },
        { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}.Type>>.self)
          types.assertTypesAreEqual()
        },
        { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<${upper_bound}>>.self)
          types.assertTypesAreEqual()
        },
        { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<Array<Array<${upper_bound}>?>>.self)
          types.assertTypesAreEqual()
        },
        { x in
          var types = SwiftTypePair(typeOf: x, type2: SwiftType<() -> (${upper_bound})?>.self)
          types.assertTypesAreEqual()
        }
      ],
      type2: SwiftType<[String : () -> (${upper_bound}, ${upper_bound})]>.self
    )
    types.assertTypesAreEqual()
  }
}

// Contravariant references.
do {
  protocol P {
    associatedtype A

    func contravariant1(_: ${type}?)
    func contravariant2(_: () -> ${type})
    func contravariant3(_: Array<() -> ${type}>)
    func contravariant4(_: [String : () -> ${type}])
    func contravariant5(_: () -> (${type}, ${type}))
    func contravariant6(_: ((${type}) -> Void) -> Void)
    func contravariant7() -> (${type}) -> Void
    func contravariant8() -> Array<((${type}) -> Void)?>
    func contravariant9(_: [String : (() -> ${type})?])
    func contravariant10() -> (Array<[String : ${type}??]>) -> Void
    func contravariant11(_: ${type}.Type)

    var contravariantProp1: (${type}?) -> Void { get }
    var contravariantProp2: (() -> ${type}) -> Void { get }
    var contravariantProp3: (Array<() -> ${type}>) -> Void { get }
    var contravariantProp4: ([String : () -> ${type}]) -> Void { get }
    var contravariantProp5: (() -> (${type}, ${type})) -> Void { get }
    var contravariantProp6: (((${type}) -> Void) -> Void) -> Void { get }
    var contravariantProp7: (${type}) -> Void { get }
    var contravariantProp8: Array<((${type}) -> Void)?> { get }
    var contravariantProp9: ([String : (() -> ${type})?]) -> Void { get }
    var contravariantProp10: (Array<[String : ${type}??]>) -> Void { get }
    var contravariantProp11: (${type}.Type) -> Void { get }

    subscript(contravariantSubscript1 _: ${type}?) -> Void { get }
    subscript(contravariantSubscript2 _: () -> ${type}) -> Void { get }
    subscript(contravariantSubscript3 _: Array<() -> ${type}>) -> Void { get }
    subscript(contravariantSubscript4 _: [String : () -> ${type}]) -> Void { get }
    subscript(contravariantSubscript5 _: () -> (${type}, ${type})) -> Void { get }
    subscript(contravariantSubscript6 _: ((${type}) -> Void) -> Void) -> Void { get }
    subscript(contravariantSubscript7 _: Void) -> (${type}) -> Void { get }
    subscript(contravariantSubscript8 _: Void) -> Array<((${type}) -> Void)?> { get }
    subscript(contravariantSubscript9 _: [String : (() -> ${type})?]) -> Void { get }
    subscript(contravariantSubscript10 _: Void) -> (Array<[String : ${type}??]>) -> Void { get }
    subscript(contravariantSubscript11 _: ${type}.Type) -> Void { get }
  }

  let exist: any P

  exist.contravariant1(0) // expected-error {{member 'contravariant1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant2(0) // expected-error {{member 'contravariant2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant3(0) // expected-error {{member 'contravariant3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant4(0) // expected-error {{member 'contravariant4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant5(0) // expected-error {{member 'contravariant5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant6(0) // expected-error {{member 'contravariant6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant7() // expected-error {{member 'contravariant7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant8() // expected-error {{member 'contravariant8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant9(0) // expected-error {{member 'contravariant9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant10() // expected-error {{member 'contravariant10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariant11(0) // expected-error {{member 'contravariant11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist.contravariantProp1 // expected-error {{member 'contravariantProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp2 // expected-error {{member 'contravariantProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp3 // expected-error {{member 'contravariantProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp4 // expected-error {{member 'contravariantProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp5 // expected-error {{member 'contravariantProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp6 // expected-error {{member 'contravariantProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp7 // expected-error {{member 'contravariantProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp8 // expected-error {{member 'contravariantProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp9 // expected-error {{member 'contravariantProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp10 // expected-error {{member 'contravariantProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.contravariantProp11 // expected-error {{member 'contravariantProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist[contravariantSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript2: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript3: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript7: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript9: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript10: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[contravariantSubscript11: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
}

// Invariant references.
do {
  protocol P {
    associatedtype A

    func invariant1(_: inout ${type})
    func invariant2(_: (inout ${type}) -> Void)
    func invariant3(_: inout Array<() -> ${type}>)
    func invariant4(_: Struct<${type}>)
    func invariant5() -> Struct<${type}>
    func invariant6() -> Struct<${type}>.Inner
    func invariant7(_: (Struct<${type}>) -> Void)
    func invariant8(_: Struct<(${type}) -> Void>)
    func invariant9(_: Struct<() -> ${type}>)
    func invariant10(_: any P & Class<${type}>)
    func invariant11() -> Struct<${type}>.InnerGeneric<Void>
    // https://github.com/apple/swift/issues/61934
    func invariant12() -> any Sequence<${type}>
    // FIXME
    // expected-error@+1 {{non-protocol, non-class type 'Sequence<${type}>' cannot be used within a protocol-constrained type}}
    func invariant13() -> any P & Sequence<${type}>
    func invariant14() -> (any Sequence<${type}>).Type


    var invariantProp1: (inout ${type}) -> Void { get }
    var invariantProp2: ((inout ${type}) -> Void) -> Void { get }
    var invariantProp3: (inout Array<() -> ${type}>) -> Void { get }
    var invariantProp4: (Struct<${type}>) -> Void { get }
    var invariantProp5: Struct<${type}> { get }
    var invariantProp6: Struct<${type}>.Inner { get }
    var invariantProp7: ((Struct<${type}>) -> Void) -> Void { get }
    var invariantProp8: (Struct<(${type}) -> Void>) -> Void { get }
    var invariantProp9: (Struct<() -> ${type}>) -> Void { get }
    var invariantProp10: (any P & Class<${type}>) -> Void { get }
    var invariantProp11: Struct<${type}>.InnerGeneric<Void> { get }

    subscript(invariantSubscript1 _: Struct<${type}>) -> Void { get }
    subscript(invariantSubscript2 _: Void) -> Struct<${type}> { get }
    subscript(invariantSubscript3 _: Void) -> Struct<${type}>.Inner { get }
    subscript(invariantSubscript4 _: (Struct<${type}>) -> Void) -> Void { get }
    subscript(invariantSubscript5 _: Struct<(${type}) -> Void>) -> Void { get }
    subscript(invariantSubscript6 _: Struct<() -> ${type}>) -> Void { get }
    subscript(invariantSubscript7 _: any P & Class<${type}>) -> Void { get }
    subscript(invariantSubscript8 _: Void) -> Struct<${type}>.InnerGeneric<Void> { get }
  }

  let exist: any P

  exist.invariant1(0) // expected-error {{member 'invariant1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant2(0) // expected-error {{member 'invariant2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant3(0) // expected-error {{member 'invariant3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant4(0) // expected-error {{member 'invariant4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant5() // expected-error {{member 'invariant5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant6() // expected-error {{member 'invariant6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant7(0) // expected-error {{member 'invariant7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant8(0) // expected-error {{member 'invariant8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant9(0) // expected-error {{member 'invariant9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant10(0) // expected-error {{member 'invariant10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariant11() // expected-error {{member 'invariant11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  do {
    var types = SwiftTypePair(typeOf: exist.invariant12(), type2: SwiftType<any Sequence>.self)
    types.assertTypesAreEqual()
  }
  do {
    var types = SwiftTypePair(typeOf: exist.invariant13(), type2: SwiftType<any P & Sequence>.self)
    types.assertTypesAreEqual()
  }
  do {
    // FIXME: Because (any P<X>).Type to (any P).Type upcast is not supported.
    // expected-error@+1 {{member 'invariant14()' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    var types = SwiftTypePair(typeOf: exist.invariant14(), type2: SwiftType<(any Sequence).Type>.self)
    types.assertTypesAreEqual()
  }

  exist.invariantProp1 // expected-error {{member 'invariantProp1' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp2 // expected-error {{member 'invariantProp2' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp3 // expected-error {{member 'invariantProp3' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp4 // expected-error {{member 'invariantProp4' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp5 // expected-error {{member 'invariantProp5' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp6 // expected-error {{member 'invariantProp6' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp7 // expected-error {{member 'invariantProp7' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp8 // expected-error {{member 'invariantProp8' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp9 // expected-error {{member 'invariantProp9' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp10 // expected-error {{member 'invariantProp10' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist.invariantProp11 // expected-error {{member 'invariantProp11' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  exist[invariantSubscript1: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript2: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript3: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript4: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript5: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript6: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript7: 0] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  exist[invariantSubscript8: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
}

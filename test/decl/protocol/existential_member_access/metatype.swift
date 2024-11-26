// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct Struct<T> {}

protocol TypeMemberOnInstanceAndViceVersa {
  static func static_covariantSelfMethod() -> Self
  static var static_covariantSelfProp: Self { get }
  static subscript(static_covariantSelfSubscript _: Void) -> Self { get }

  static func static_invariantSelfMethod() -> Struct<Self>
  static var static_invariantSelfProp: Struct<Self> { get }
  static subscript(static_invariantSelfSubscript _: Void) -> Struct<Self> { get }

  func covariantSelfMethod() -> Self

  func invariantSelfMethod() -> Struct<Self>
  var invariantSelfProp: Struct<Self> { get }
  subscript(invariantSelfSubscript _: Void) -> Struct<Self> { get }
}
// Test that invalid reference errors prevail over unsupported existential
// member accesses.
do {
  let protoMeta: (any TypeMemberOnInstanceAndViceVersa).Type
  let existMeta: any TypeMemberOnInstanceAndViceVersa.Type
  let instance: any TypeMemberOnInstanceAndViceVersa

  // TypeMemberOnInstanceAndViceVersa.Protocol
  protoMeta.static_invariantSelfMethod() // expected-error {{static member 'static_invariantSelfMethod' cannot be used on protocol metatype '(any TypeMemberOnInstanceAndViceVersa).Type'}}
  protoMeta.static_invariantSelfProp // expected-error {{static member 'static_invariantSelfProp' cannot be used on protocol metatype '(any TypeMemberOnInstanceAndViceVersa).Type'}}
  protoMeta[static_invariantSelfSubscript: ()] // expected-error {{static member 'subscript' cannot be used on protocol metatype '(any TypeMemberOnInstanceAndViceVersa).Type'}}
  _ = protoMeta.covariantSelfMethod // ok
  protoMeta.invariantSelfMethod // expected-error {{member 'invariantSelfMethod' cannot be used on value of type '(any TypeMemberOnInstanceAndViceVersa).Type'; consider using a generic constraint instead}}
  protoMeta.invariantSelfProp // expected-error {{instance member 'invariantSelfProp' cannot be used on type 'any TypeMemberOnInstanceAndViceVersa'}}
  protoMeta[invariantSelfSubscript: ()] // expected-error {{instance member 'subscript' cannot be used on type 'any TypeMemberOnInstanceAndViceVersa'}}

  // TypeMemberOnInstanceAndViceVersa.Type
  _ = existMeta.static_covariantSelfMethod // ok
  _ = existMeta.static_covariantSelfProp // ok
  _ = existMeta[static_covariantSelfSubscript: ()] // ok
  existMeta.static_invariantSelfMethod // expected-error {{member 'static_invariantSelfMethod' cannot be used on value of type 'any TypeMemberOnInstanceAndViceVersa.Type'; consider using a generic constraint instead}}
  existMeta.static_invariantSelfProp // expected-error {{member 'static_invariantSelfProp' cannot be used on value of type 'any TypeMemberOnInstanceAndViceVersa.Type'; consider using a generic constraint instead}}
  existMeta[static_invariantSelfSubscript: ()] // expected-error {{member 'subscript' cannot be used on value of type 'any TypeMemberOnInstanceAndViceVersa.Type'; consider using a generic constraint instead}}
  existMeta.invariantSelfMethod // expected-error {{instance member 'invariantSelfMethod' cannot be used on type 'TypeMemberOnInstanceAndViceVersa'}}
  existMeta.invariantSelfProp // expected-error {{instance member 'invariantSelfProp' cannot be used on type 'TypeMemberOnInstanceAndViceVersa'}}
  existMeta[invariantSelfSubscript: ()] // expected-error {{instance member 'subscript' cannot be used on type 'TypeMemberOnInstanceAndViceVersa'}}

  // TypeMemberOnInstanceAndViceVersa
  instance.static_invariantSelfMethod // expected-error {{static member 'static_invariantSelfMethod' cannot be used on instance of type 'any TypeMemberOnInstanceAndViceVersa'}}
  instance.static_invariantSelfProp // expected-error {{static member 'static_invariantSelfProp' cannot be used on instance of type 'any TypeMemberOnInstanceAndViceVersa'}}
  instance[static_invariantSelfSubscript: ()] // expected-error {{static member 'subscript' cannot be used on instance of type 'any TypeMemberOnInstanceAndViceVersa'}}
}

// Test that covariant erasure turns metatypes into existential metatypes.
protocol CovariantMetatypes {
  func covariantSelfMetatype1(_: (Self.Type.Type.Type) -> Void)
  func covariantSelfMetatype2() -> (Self.Type, Self.Type.Type)

  var covariantSelfMetatypeProp1: Self.Type.Type.Type { get }
  var covariantSelfMetatypeProp2: (Self.Type, Self.Type.Type) { get }

  subscript(covariantSelfMetatypeSubscript1 _: (Self.Type.Type.Type) -> Void) -> Self.Type { get }
  subscript(covariantSelfMetatypeSubscript2 _: Void) -> (Self.Type, Self.Type.Type) { get }

  associatedtype A

  func covariantAssocMetatype1(_: (A.Type.Type.Type) -> Void)
  func covariantAssocMetatype2() -> (A.Type, A.Type.Type)

  var covariantAssocMetatypeProp1: A.Type.Type.Type { get }
  var covariantAssocMetatypeProp2: (A.Type, A.Type.Type) { get }

  subscript(covariantAssocMetatypeSubscript1 _: (A.Type.Type.Type) -> Void) -> A.Type { get }
  subscript(covariantAssocMetatypeSubscript2 _: Void) -> (A.Type, A.Type.Type) { get }
}
do {
  func testCovariantMetatypes(arg: any CovariantMetatypes) {
    arg.covariantSelfMetatype1 { (_: any CovariantMetatypes.Type.Type.Type) in }
    let _: (any CovariantMetatypes.Type, any CovariantMetatypes.Type.Type) = arg.covariantSelfMetatype2()

    let _: any CovariantMetatypes.Type.Type.Type = arg.covariantSelfMetatypeProp1
    let _: (any CovariantMetatypes.Type, any CovariantMetatypes.Type.Type) = arg.covariantSelfMetatypeProp2

    let _: any CovariantMetatypes.Type = arg[covariantSelfMetatypeSubscript1: { (_: any CovariantMetatypes.Type.Type.Type) in }]
    let _: (any CovariantMetatypes.Type, any CovariantMetatypes.Type.Type) = arg[covariantSelfMetatypeSubscript2: ()]

    arg.covariantAssocMetatype1 { (_: Any.Type.Type.Type) in }
    let _: (Any.Type, Any.Type.Type) = arg.covariantAssocMetatype2()

    let _: Any.Type.Type.Type = arg.covariantAssocMetatypeProp1
    let _: (Any.Type, Any.Type.Type) = arg.covariantAssocMetatypeProp2

    let _: Any.Type = arg[covariantAssocMetatypeSubscript1: { (_: Any.Type.Type.Type) in }]
    let _: (Any.Type, Any.Type.Type) = arg[covariantAssocMetatypeSubscript2: ()]
  }
}

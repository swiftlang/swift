// RUN: %target-typecheck-verify-swift

struct OtherGeneric<U> {}

struct Constrained<T> {
  // expected-note@-1 12{{'T' declared as parameter to type 'Constrained'}}
  // expected-note@-2 10{{generic struct 'Constrained' declared here}}
  typealias NonGeneric = Int where T == Int // expected-note {{requirement specified as 'T' == 'Int'}}
  typealias FakeGeneric = T where T == Int // expected-note {{requirement specified as 'T' == 'Int'}}

  typealias Unbound = OtherGeneric where T == Int
  typealias Generic/*<U>*/ = OtherGeneric/*<U>*/ where T == Int // FIXME: Adding the generic params crashes the compiler
}

extension Constrained where T == Int { // expected-note 2{{requirement specified as 'T' == 'Int'}}
  typealias NonGenericInExtension = Int
  typealias FakeGenericInExtension = T

  typealias UnboundInExtension = OtherGeneric
  typealias GenericInExtension = OtherGeneric
}

struct Unconstrained<T> {
  // expected-note@-1 4{{generic struct 'Unconstrained' declared here}}
  // expected-note@-2 9{{'T' declared as parameter to type 'Unconstrained'}}
  typealias NonGeneric = Int
  typealias GenericParam = T

  typealias Unbound = OtherGeneric
  typealias Generic/*<U>*/ = OtherGeneric/*<U>*/ // FIXME: Adding the generic params crashes the compiler
}

// Referencing an unbound generic parameter through a typealias is always invalid.
func invalid(
  _: Unconstrained.GenericParam, // expected-error {{reference to generic type 'Unconstrained' requires arguments in <...>}}
) {
  let _ = Unconstrained.GenericParam.self
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  // expected-note@-2 {{explicitly specify the generic arguments to fix this issue}}
  let _ = Unconstrained<_>.GenericParam.self
  // expected-error@-1 {{could not infer type for placeholder}}
}

// But if the underlying type does not reference the generic parameter it's fine.
func use(
  _: Constrained.NonGeneric,
  _: Constrained.FakeGeneric,
  _: Constrained.Unbound<String>,
  _: Constrained.Generic<String>,
  _: Constrained.NonGenericInExtension,
  _: Constrained.UnboundInExtension<String>,
  _: Constrained.GenericInExtension<String>,
  _: Unconstrained.NonGeneric,
  _: Unconstrained.Unbound<String>,
  _: Unconstrained.Generic<String>,
) {

  // FIXME: These should all work
  let _ = Constrained.NonGeneric.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.FakeGeneric.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.Unbound<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.Generic<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}

  let _ = Constrained.NonGenericInExtension.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.FakeGenericInExtension.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.UnboundInExtension<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Constrained.GenericInExtension<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}

  let _ = Unconstrained.NonGeneric.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Unconstrained.Unbound<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _ = Unconstrained.Generic<String>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}

  let _ = Constrained<_>.NonGeneric.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.FakeGeneric.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.NonGenericInExtension.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.FakeGenericInExtension.self // expected-error {{could not infer type for placeholder}}

  let _ = Constrained<_>.Unbound<String>.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.Generic<String>.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.UnboundInExtension<String>.self // expected-error {{could not infer type for placeholder}}
  let _ = Constrained<_>.GenericInExtension<String>.self // expected-error {{could not infer type for placeholder}}

  let _ = Unconstrained<_>.NonGeneric.self // expected-error {{could not infer type for placeholder}}
  let _ = Unconstrained<_>.Unbound<String>.self // expected-error {{could not infer type for placeholder}}
  let _ = Unconstrained<_>.Generic<String>.self // expected-error {{could not infer type for placeholder}}

  let _: OtherGeneric<String>.Type = Constrained.Unbound<_>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _: OtherGeneric<String>.Type = Constrained.Generic<_>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _: OtherGeneric<String>.Type = Constrained.UnboundInExtension<_>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  let _: OtherGeneric<String>.Type = Constrained.GenericInExtension<_>.self // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}

  let _: OtherGeneric<String>.Type = Unconstrained<_>.Unbound.self // expected-error {{could not infer type for placeholder}}
  let _: OtherGeneric<String>.Type = Unconstrained<_>.Generic.self // expected-error {{could not infer type for placeholder}}

  let _: OtherGeneric<String>.Type = Constrained<_>.Unbound<_>.self // expected-error {{could not infer type for placeholder}}
  let _: OtherGeneric<String>.Type = Constrained<_>.Generic<_>.self // expected-error {{could not infer type for placeholder}}
  let _: OtherGeneric<String>.Type = Constrained<_>.UnboundInExtension<_>.self // expected-error {{could not infer type for placeholder}}
  let _: OtherGeneric<String>.Type = Constrained<_>.GenericInExtension<_>.self // expected-error {{could not infer type for placeholder}}

  let _: OtherGeneric<String>.Type = Unconstrained<_>.Unbound<_>.self // expected-error {{could not infer type for placeholder}}
  let _: OtherGeneric<String>.Type = Unconstrained<_>.Generic<_>.self // expected-error {{could not infer type for placeholder}}

  let _: Constrained.NonGeneric = 123
  let _: Constrained.FakeGeneric = 123
  let _: Constrained.NonGenericInExtension = 123
  let _: Constrained.FakeGenericInExtension = 123

  let _: Constrained.Unbound = OtherGeneric<String>()
  let _: Constrained.Generic = OtherGeneric<String>()
  let _: Constrained.UnboundInExtension = OtherGeneric<String>()
  let _: Constrained.GenericInExtension = OtherGeneric<String>()

  let _: Unconstrained.NonGeneric = 123
  let _: Unconstrained.Unbound = OtherGeneric<String>()
  let _: Unconstrained.Generic = OtherGeneric<String>()

  // FIXME: These should work
  let _: Constrained<_>.NonGeneric = 123 // expected-error {{'Constrained<T>.NonGeneric' (aka 'Int') requires the types '_' and 'Int' be equivalent}}
  let _: Constrained<_>.FakeGeneric = 123 // expected-error {{'Constrained<T>.FakeGeneric' (aka 'T') requires the types '_' and 'Int' be equivalent}}
  let _: Constrained<_>.NonGenericInExtension = 123 // expected-error {{'Constrained<T>.NonGenericInExtension' (aka 'Int') requires the types '_' and 'Int' be equivalent}}
  let _: Constrained<_>.FakeGenericInExtension = 123 // expected-error {{'Constrained<T>.FakeGenericInExtension' (aka 'T') requires the types '_' and 'Int' be equivalent}}

  let _: Unconstrained<_>.NonGeneric = 123 // expected-error {{could not infer type for placeholder}}
  let _: Unconstrained<_>.Unbound = OtherGeneric<String>()
  let _: Unconstrained<_>.Generic = OtherGeneric<String>()

  let _: Constrained<_>.Unbound = OtherGeneric<String>()
  let _: Constrained<_>.Generic = OtherGeneric<String>()
  let _: Constrained<_>.UnboundInExtension = OtherGeneric<String>()
  let _: Constrained<_>.GenericInExtension = OtherGeneric<String>()

  let _: Constrained.Unbound<_> = OtherGeneric<String>()
  let _: Constrained.Generic<_> = OtherGeneric<String>()
  let _: Constrained.UnboundInExtension<_> = OtherGeneric<String>()
  let _: Constrained.GenericInExtension<_> = OtherGeneric<String>()

  let _: Constrained<_>.Unbound<_> = OtherGeneric<String>()
  let _: Constrained<_>.Generic<_> = OtherGeneric<String>()
  let _: Constrained<_>.UnboundInExtension<_> = OtherGeneric<String>()
  let _: Constrained<_>.GenericInExtension<_> = OtherGeneric<String>()

  let _: Unconstrained<_>.Unbound<_> = OtherGeneric<String>()
  let _: Unconstrained<_>.Generic<_> = OtherGeneric<String>()

  // FIXME: These should all work
  _ = { (_:Unconstrained.NonGeneric) in }(123)
  _ = { (_:Unconstrained<_>.NonGeneric) in }(123) // expected-error {{could not infer type for placeholder}}
  _ = { (_:Unconstrained.Unbound) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained.Generic) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained<_>.Unbound) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained<_>.Generic) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained.Unbound<_>) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained.Generic<_>) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained<_>.Unbound<_>) in }(OtherGeneric<String>())
  _ = { (_:Unconstrained<_>.Generic<_>) in }(OtherGeneric<String>())

  _ = { () -> Unconstrained.NonGeneric in 123 } // expected-error {{generic parameter 'T' could not be inferred}}
  _ = { () -> Unconstrained<_>.NonGeneric in 123 }  // expected-error {{could not infer type for placeholder}}
  _ = { () -> Unconstrained.Unbound in OtherGeneric<String>() } // expected-error {{generic parameter 'T' could not be inferred}}
  _ = { () -> Unconstrained.Generic in OtherGeneric<String>() } // expected-error {{generic parameter 'T' could not be inferred}}
  _ = { () -> Unconstrained<_>.Unbound in OtherGeneric<String>() } // expected-error {{could not infer type for placeholder}}
  _ = { () -> Unconstrained<_>.Generic in OtherGeneric<String>() } // expected-error {{could not infer type for placeholder}}
  _ = { () -> Unconstrained.Unbound<_> in OtherGeneric<String>() } // expected-error {{generic parameter 'T' could not be inferred}}
  _ = { () -> Unconstrained.Generic<_> in OtherGeneric<String>() } // expected-error {{generic parameter 'T' could not be inferred}}
  _ = { () -> Unconstrained<_>.Unbound<_> in OtherGeneric<String>() }  // expected-error {{could not infer type for placeholder}}
  _ = { () -> Unconstrained<_>.Generic<_> in OtherGeneric<String>() }  // expected-error {{could not infer type for placeholder}}
}

struct Use {
  let a1: Constrained.NonGeneric
  let b1: Constrained.FakeGeneric
  let c1: Constrained.Unbound<String>
  let d1: Constrained.Generic<String>
  let a2: Constrained.NonGenericInExtension
  let b2: Constrained.FakeGenericInExtension
  let c2: Constrained.UnboundInExtension<String>
  let d2: Constrained.GenericInExtension<String>
  let e1: Unconstrained.NonGeneric
  let e2: Unconstrained.Unbound<String>
  let e3: Unconstrained.Generic<String>
}

extension Constrained.NonGeneric {}
extension Constrained.Unbound {}
extension Constrained.Generic {}

extension Constrained.NonGenericInExtension {}
extension Constrained.UnboundInExtension {}
extension Constrained.GenericInExtension {}

extension Unconstrained.NonGeneric {}
extension Unconstrained.Unbound {}
extension Unconstrained.Generic {}

typealias Alias1 = Constrained.NonGeneric // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias2 = Constrained.Unbound // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias3 = Constrained.Generic // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias4 = Constrained.Unbound<String> // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias5 = Constrained.Generic<String> // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}

typealias Alias6 = Constrained<_>.NonGeneric // expected-error {{type placeholder not allowed here}}
typealias Alias7 = Constrained<_>.Unbound // expected-error {{type placeholder not allowed here}}
typealias Alias8 = Constrained<_>.Generic // expected-error {{type placeholder not allowed here}}
typealias Alias9 = Constrained<_>.Unbound<String> // expected-error {{type placeholder not allowed here}}
typealias Alias10 = Constrained<_>.Generic<String> // expected-error {{type placeholder not allowed here}}

typealias Alias11 = Constrained.NonGenericInExtension // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias12 = Constrained.UnboundInExtension // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias13 = Constrained.GenericInExtension // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias14 = Constrained.UnboundInExtension<String> // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}
typealias Alias15 = Constrained.GenericInExtension<String> // expected-error {{reference to generic type 'Constrained' requires arguments in <...>}}

typealias Alias16 = Unconstrained.NonGeneric // expected-error {{reference to generic type 'Unconstrained' requires arguments in <...>}}
typealias Alias17 = Unconstrained.Unbound // expected-error {{reference to generic type 'Unconstrained' requires arguments in <...>}}
typealias Alias18 = Unconstrained.Generic // expected-error {{reference to generic type 'Unconstrained' requires arguments in <...>}}
typealias Alias19 = Unconstrained<_>.Unbound<_> // expected-error {{type placeholder not allowed here}}
typealias Alias20 = Unconstrained<_>.Generic<_> // expected-error {{type placeholder not allowed here}}

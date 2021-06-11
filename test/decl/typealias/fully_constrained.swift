// RUN: %target-typecheck-verify-swift

struct OtherGeneric<U> {}

struct Generic<T> {
  // FIXME: Should work with 'T' as well
  typealias NonGeneric = Int where T == Int

  typealias Unbound = OtherGeneric where T == Int
  typealias Generic = OtherGeneric where T == Int
}

extension Generic where T == Int {
  // FIXME: Should work with 'T' as well
  typealias NonGenericInExtension = Int

  typealias UnboundInExtension = OtherGeneric
  typealias GenericInExtension = OtherGeneric
}

func use(_: Generic.NonGeneric,
         _: Generic.Unbound<String>,
         _: Generic.Generic<String>,
         _: Generic.NonGenericInExtension,
         _: Generic.UnboundInExtension<String>,
         _: Generic.GenericInExtension<String>) {

  // FIXME: Get these working too
#if false
  let _ = Generic.NonGeneric.self
  let _ = Generic.Unbound<String>.self
  let _ = Generic.Generic<String>.self

  let _ = Generic.NonGenericInExtension.self
  let _ = Generic.UnboundInExtension<String>.self
  let _ = Generic.GenericInExtension<String>.self

  let _: Generic.NonGeneric = 123
  let _: Generic.NonGenericInExtension = 123
#endif

  let _: Generic.Unbound = OtherGeneric<String>()
  let _: Generic.Generic = OtherGeneric<String>()

  let _: Generic.UnboundInExtension = OtherGeneric<String>()
  let _: Generic.GenericInExtension = OtherGeneric<String>()
}

struct Use {
  let a1: Generic.NonGeneric
  let b1: Generic.Unbound<String>
  let c1: Generic.Generic<String>
  let a2: Generic.NonGenericInExtension
  let b2: Generic.UnboundInExtension<String>
  let c2: Generic.GenericInExtension<String>
}

extension Generic.NonGeneric {}
extension Generic.Unbound {}
extension Generic.Generic {}

extension Generic.NonGenericInExtension {}
extension Generic.UnboundInExtension {}
extension Generic.GenericInExtension {}

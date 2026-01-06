// RUN: %target-typecheck-verify-swift

struct OtherGeneric<U> {}

struct Generic<T> {
  typealias NonGeneric = Int where T == Int
  typealias FakeGeneric = T where T == Int

  typealias Unbound = OtherGeneric where T == Int
  typealias Generic = OtherGeneric where T == Int
}

extension Generic where T == Int {
  typealias NonGenericInExtension = Int
  typealias FakeGenericInExtension = T

  typealias UnboundInExtension = OtherGeneric
  typealias GenericInExtension = OtherGeneric
}

func use(_: Generic.NonGeneric,
         _: Generic.FakeGeneric,
         _: Generic.Unbound<String>,
         _: Generic.Generic<String>,
         _: Generic.NonGenericInExtension,
         _: Generic.UnboundInExtension<String>,
         _: Generic.GenericInExtension<String>) {

  // FIXME: Get these working too
#if false
  let _ = Generic.NonGeneric.self
  let _ = Generic.FakeGeneric.self
  let _ = Generic.Unbound<String>.self
  let _ = Generic.Generic<String>.self

  let _ = Generic.NonGenericInExtension.self
  let _ = Generic.FakeGenericInExtension.self
  let _ = Generic.UnboundInExtension<String>.self
  let _ = Generic.GenericInExtension<String>.self
#endif

  let _: Generic.NonGeneric = 123
  let _: Generic.FakeGeneric = 123
  let _: Generic.NonGenericInExtension = 123
  let _: Generic.FakeGenericInExtension = 123

  let _: Generic.Unbound = OtherGeneric<String>()
  let _: Generic.Generic = OtherGeneric<String>()

  let _: Generic.UnboundInExtension = OtherGeneric<String>()
  let _: Generic.GenericInExtension = OtherGeneric<String>()
}

struct Use {
  let a1: Generic.NonGeneric
  let b1: Generic.FakeGeneric
  let c1: Generic.Unbound<String>
  let d1: Generic.Generic<String>
  let a2: Generic.NonGenericInExtension
  let b2: Generic.FakeGenericInExtension
  let c2: Generic.UnboundInExtension<String>
  let d2: Generic.GenericInExtension<String>
}

extension Generic.NonGeneric {}
extension Generic.Unbound {}
extension Generic.Generic {}

extension Generic.NonGenericInExtension {}
extension Generic.UnboundInExtension {}
extension Generic.GenericInExtension {}

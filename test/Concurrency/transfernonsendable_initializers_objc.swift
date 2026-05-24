// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -swift-version 6 -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: objc_interop

// This test validates the behavior of transfernonsendable around NSObject-subclassing
// actor initializers. NSObject actors have a distinctive SIL codegen: properties are
// assigned *before* the objc_super_method call to NSObject.init, and `self` is held
// on a stack slot that is updated with the result of super.init. The region isolation
// analysis recognizes the stack slot as the self box and marks any ref_element_addr
// derived from it as actor-instance-isolated.

import Foundation

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}
struct NoncopyableAccount: ~Copyable {}

// Copyable struct and enum containers that embed a non-sendable reference.
struct StructWrapper { var account: NonSendableKlass }
enum EnumWrapper { case some(NonSendableKlass), none }
indirect enum IndirectEnumWrapper { case leaf(NonSendableKlass), branch(IndirectEnumWrapper, IndirectEnumWrapper) }

// Noncopyable struct and enum containers that embed a non-sendable reference.
struct NoncopyableStructWrapper: ~Copyable { var account: NonSendableKlass }
enum NoncopyableEnumWrapper: ~Copyable { case some(NonSendableKlass), none }

/////////////////
// MARK: Tests //
/////////////////

// MARK: - let properties

actor Task1: NSObject {
  let account: NonSendableKlass
  init(account: NonSendableKlass) {
    self.account = account
  }
}

actor Task2<T>: NSObject {
  let account: T
  init(account: T) {
    self.account = account
  }
}

actor Task3: NSObject {
  let account: NoncopyableAccount
  init(account: consuming NoncopyableAccount) {
    self.account = account
  }
}

actor Task4<T: ~Copyable>: NSObject {
  let account: T
  init(account: consuming T) {
    self.account = account
  }
}

actor Task5<T: AnyObject>: NSObject {
  let account: T
  init(account: T) {
    self.account = account
  }
}

// Multiple let properties assigned in reversed declaration order.
actor Task6: NSObject {
  let a: NonSendableKlass
  let b: NonSendableKlass
  init(a: NonSendableKlass, b: NonSendableKlass) {
    self.b = b
    self.a = a
  }
}

// Sendable default-initialized property alongside a non-sendable let.
actor Task7: NSObject {
  var count: Int = 0
  let account: NonSendableKlass
  init(account: NonSendableKlass) {
    self.account = account
  }
}

// Multiple generic type parameters with distinct constraints.
actor Task8<T: ~Copyable, U: AnyObject>: NSObject {
  let item: T
  let ref: U
  init(item: consuming T, ref: U) {
    self.item = item
    self.ref = ref
  }
}

// MARK: - var, optional, weak, and failable-init properties

actor Task9: NSObject {
  var account: NonSendableKlass
  init(account: NonSendableKlass) {
    self.account = account
  }
}

actor Task10: NSObject {
  var account: NonSendableKlass?
  init(account: NonSendableKlass?) {
    self.account = account
  }
}

actor Task11: NSObject {
  let account: NonSendableKlass
  init?(account: NonSendableKlass?) {
    guard let account else { return nil }
    self.account = account
  }
}

actor Task12: NSObject {
  weak var account: NonSendableKlass?
  init(account: NonSendableKlass?) {
    self.account = account
  }
}

// MARK: - Struct field types

// Copyable struct wrapping a non-sendable.
actor Task13: NSObject {
  let w: StructWrapper
  init(w: StructWrapper) {
    self.w = w
  }
}

// Generic copyable.
actor Task14<T>: NSObject {
  let w: T
  init(w: T) {
    self.w = w
  }
}

// Generic noncopyable.
actor Task15<T: ~Copyable>: NSObject {
  let w: T
  init(w: consuming T) {
    self.w = w
  }
}

// Concrete noncopyable struct with a non-sendable field.
actor Task16: NSObject {
  let w: NoncopyableStructWrapper
  init(w: consuming NoncopyableStructWrapper) {
    self.w = w
  }
}

// MARK: - Enum field types

// Copyable enum with non-sendable payload.
actor Task17: NSObject {
  let status: EnumWrapper
  init(status: EnumWrapper) {
    self.status = status
  }
}

// Generic copyable.
actor Task18<T>: NSObject {
  let status: T
  init(status: T) {
    self.status = status
  }
}

// Generic noncopyable.
actor Task19<T: ~Copyable>: NSObject {
  let status: T
  init(status: consuming T) {
    self.status = status
  }
}

// Concrete noncopyable enum with a non-sendable payload.
actor Task20: NSObject {
  let status: NoncopyableEnumWrapper
  init(status: consuming NoncopyableEnumWrapper) {
    self.status = status
  }
}

// MARK: - Indirect enum field types

// Indirect enum with non-sendable payload.
actor Task21: NSObject {
  let tree: IndirectEnumWrapper
  init(tree: IndirectEnumWrapper) {
    self.tree = tree
  }
}

// MARK: - Tuple field types

// Both elements non-sendable.
actor Task22: NSObject {
  let pair: (NonSendableKlass, NonSendableKlass)
  init(pair: (NonSendableKlass, NonSendableKlass)) {
    self.pair = pair
  }
}

// One non-sendable element, one Sendable.
actor Task23: NSObject {
  let pair: (NonSendableKlass, Int)
  init(pair: (NonSendableKlass, Int)) {
    self.pair = pair
  }
}

// Three non-sendable elements.
actor Task25: NSObject {
  let triple: (NonSendableKlass, NonSendableKlass, NonSendableKlass)
  init(triple: (NonSendableKlass, NonSendableKlass, NonSendableKlass)) {
    self.triple = triple
  }
}

// Three elements, mixed sendable.
actor Task26: NSObject {
  let triple: (NonSendableKlass, Int, NonSendableKlass)
  init(triple: (NonSendableKlass, Int, NonSendableKlass)) {
    self.triple = triple
  }
}

// sending tuple parameter.
actor Task28: NSObject {
  let pair: (NonSendableKlass, NonSendableKlass)
  init(pair: sending (NonSendableKlass, NonSendableKlass)) {
    self.pair = pair
  }
}

// Tuple field assigned from freshly-allocated objects.
actor Task30: NSObject {
  let pair: (NonSendableKlass, NonSendableKlass)
  override init() {
    self.pair = (NonSendableKlass(), NonSendableKlass())
  }
}

// Swapped assignment `self.pair = (b, a)`.
actor Task27: NSObject {
  let pair: (NonSendableKlass, NonSendableKlass)
  init(a: NonSendableKlass, b: NonSendableKlass) {
    self.pair = (b, a)
  }
}

// Nested tuple literal `((a, b), c)`.
actor Task31: NSObject {
  let nested: ((NonSendableKlass, NonSendableKlass), NonSendableKlass)
  init(a: NonSendableKlass, b: NonSendableKlass, c: NonSendableKlass) {
    self.nested = ((a, b), c)
  }
}

// Deeply nested tuple literal `((a, b), (c, d))`.
actor Task32: NSObject {
  let deep: ((NonSendableKlass, NonSendableKlass), (NonSendableKlass, NonSendableKlass))
  init(a: NonSendableKlass, b: NonSendableKlass, c: NonSendableKlass, d: NonSendableKlass) {
    self.deep = ((a, b), (c, d))
  }
}

// inout tuple parameter.
actor Task29: NSObject {
  let pair: (NonSendableKlass, NonSendableKlass)
  init(pair: inout (NonSendableKlass, NonSendableKlass)) {
    self.pair = pair
  }
}

// Generic tuple (A, B).
actor Task24<A, B>: NSObject {
  let pair: (A, B)
  init(pair: (A, B)) {
    self.pair = pair
  }
}

// MARK: - More generic tuple variants

// One generic element, one concrete non-sendable.
actor Task36<T>: NSObject {
  let pair: (T, NonSendableKlass)
  init(t: T, ns: NonSendableKlass) {
    self.pair = (t, ns)
  }
}

// Homogeneous generic pair.
actor Task37<T>: NSObject {
  let pair: (T, T)
  init(a: T, b: T) {
    self.pair = (a, b)
  }
}

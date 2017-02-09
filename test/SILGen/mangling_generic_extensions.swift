// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

struct Foo<T> {
}

protocol Runcible {
  associatedtype Spoon
  associatedtype Hat
}

extension Foo {
  // An unconstrained extension in the same module doesn't use the extension
  // mangling, since the implementation can be resiliently moved into the
  // definition.
  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions3FooV1aSifg
  // NO-SELF-LABEL: sil hidden @_T027mangling_generic_extensions3FooV1aSifg
  var a: Int { return 0 }

  // NO-SELF-LABEL: sil hidden @_T027mangling_generic_extensions3FooV3zimyyF
  func zim() { }
  // NO-SELF-LABEL: sil hidden @_T027mangling_generic_extensions3FooV4zangyqd__lF
  func zang<U>(_: U) { }
  // NO-SELF-LABEL: sil hidden @_T027mangling_generic_extensions3FooV4zungyqd__AA8RuncibleRd__3HatQyd__Rs_lF
  func zung<U: Runcible where U.Hat == T>(_: U) { }
}

extension Foo where T: Runcible {
  // A constrained extension always uses the extension mangling.
  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions3FooVAaA8RuncibleRzlE1aSifg
  var a: Int { return 0 }

  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions3FooVAaA8RuncibleRzlE1bxfg
  var b: T { get { } }
}

extension Foo where T: Runcible, T.Spoon: Runcible {
  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions3FooVAaA8RuncibleRzAaD5SpoonRpzlE1aSifg
  var a: Int { return 0 }

  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions3FooVAaA8RuncibleRzAaD5SpoonRpzlE1bxfg
  var b: T { get { } }
}

// Protocol extensions always use the extension mangling.
// TODO: When default implementations are properly implemented, and protocol
// extensions receive dynamic dispatch, it would be possible for an
// unconstrained protocol extension method to be moved in or out of its
// declaration, so we would no longer want to use the extension mangling
// in unconstrained cases.
extension Runcible {
  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions8RunciblePAAE5runceyyF
  func runce() {}
}

extension Runcible where Self.Spoon == Self.Hat {
  // CHECK-LABEL: sil hidden @_T027mangling_generic_extensions8RunciblePAaaBRz5SpoonQz3HatRtzlE5runceyyF
  func runce() {}
}

// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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
  // CHECK-LABEL: sil hidden @_TFV27mangling_generic_extensions3Foog1aSi
  // NO-SELF-LABEL: sil hidden @_TFV27mangling_generic_extensions3Foog1aSi
  var a: Int { return 0 }

  // NO-SELF-LABEL: sil hidden @_TFV27mangling_generic_extensions3Foo3zimfT_T_
  func zim() { }
  // NO-SELF-LABEL: sil hidden @_TFV27mangling_generic_extensions3Foo4zangu_rfqd__T_
  func zang<U>(_: U) { }
  // NO-SELF-LABEL: sil hidden @_TFV27mangling_generic_extensions3Foo4zungu_Rd__S_8Runcible_zwd__3Hatrfqd__T_
  func zung<U: Runcible where U.Hat == T>(_: U) { }
}

extension Foo where T: Runcible {
  // A constrained extension always uses the extension mangling.
  // CHECK-LABEL: sil hidden @_TFe27mangling_generic_extensionsRxS_8RunciblerVS_3Foog1aSi
  var a: Int { return 0 }

  // CHECK-LABEL: sil hidden @_TFe27mangling_generic_extensionsRxS_8RunciblerVS_3Foog1bx
  var b: T { get { } }
}

extension Foo where T: Runcible, T.Spoon: Runcible {
  // CHECK-LABEL: sil hidden @_TFe27mangling_generic_extensionsRxS_8Runciblewx5SpoonS0_rVS_3Foog1aSi
  var a: Int { return 0 }

  // CHECK-LABEL: sil hidden @_TFe27mangling_generic_extensionsRxS_8Runciblewx5SpoonS0_rVS_3Foog1bx
  var b: T { get { } }
}

// Protocol extensions always use the extension mangling.
// TODO: When default implementations are properly implemented, and protocol
// extensions receive dynamic dispatch, it would be possible for an
// unconstrained protocol extension method to be moved in or out of its
// declaration, so we would no longer want to use the extension mangling
// in unconstrained cases.
extension Runcible {
  // CHECK-LABEL: sil hidden @_TFE27mangling_generic_extensionsPS_8Runcible5runcefT_T_
  func runce() {}
}

extension Runcible where Self.Spoon == Self.Hat {
  // CHECK-LABEL: sil hidden @_TFe27mangling_generic_extensionsRxS_8Runciblewx3Hatzwx5SpoonrS0_5runcefT_T_
  func runce() {}
}

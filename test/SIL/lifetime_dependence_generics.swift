// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -disable-experimental-parser-round-trip \
// RUN: | %FileCheck %s
// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

protocol P {
  associatedtype E: ~Escapable
  @lifetime(borrow self)
  borrowing func getE() -> E
}

extension P {
  @lifetime(borrow self)
  borrowing func getDefault() -> E {
    return getE()
  }
}

public struct View: ~Escapable {
  // TODO: dependsOn(immortal)
  @_unsafeNonescapableResult
  init() { }
}

public struct PView: P {
  borrowing func getE() -> View { return View() }
}

public func test(pview: borrowing PView) -> View {
  return pview.getDefault()
}

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics1PPAAE10getDefault1EQzyF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @lifetime(borrow 0)  @out Self.E {

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics5PViewV4getEAA4ViewVyF : $@convention(method) (PView) -> @lifetime(borrow 0)  @owned View {

// CHECK-LABEL: sil private [transparent] [thunk] @$s28lifetime_dependence_generics5PViewVAA1PA2aDP4getE1EQzyFTW : $@convention(witness_method: P) (@in_guaranteed PView) -> @lifetime(borrow 0)  @out View {

// CHECK-LABEL: sil @$s28lifetime_dependence_generics4test5pviewAA4ViewVAA5PViewV_tF : $@convention(thin) (PView) -> @lifetime(borrow 0)  @owned View {

// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-experimental-lifetime-dependence-inference | %FileCheck %s
// REQUIRES: noncopyable_generics

protocol P {
  associatedtype E: ~Escapable
  borrowing func getE() -> _borrow(self) E
}

extension P {
  borrowing func getDefault() -> _borrow(self) E {
    return getE()
  }
}

public struct View: ~Escapable {
  @_unsafeNonescapableResult
  init() { }
}

public struct PView: P {
  borrowing func getE() -> _borrow(self) View { return View() }
}

public func test(pview: consuming PView) -> _consume(pview) View {
  return pview.getDefault()
}

// CHECK: sil hidden @$s28lifetime_dependence_generics1PPAAE10getDefault1EQzyF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> _scope(0) @out Self.E {

// CHECK: sil hidden @$s28lifetime_dependence_generics5PViewV4getEAA4ViewVyF : $@convention(method) (PView) -> _scope(0) @owned View {

// CHECK: sil private [transparent] [thunk] @$s28lifetime_dependence_generics5PViewVAA1PA2aDP4getE1EQzyFTW : $@convention(witness_method: P) (@in_guaranteed PView) -> _scope(0) @out View {

// CHECK: sil @$s28lifetime_dependence_generics4test5pviewAA4ViewVAA5PViewVn_tF : $@convention(thin) (PView) -> _inherit(1) @owned View {

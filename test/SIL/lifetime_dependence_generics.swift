// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes | %FileCheck %s


protocol P {
  associatedtype E: ~Escapable
  borrowing func getE() -> dependsOn(self) E
}

extension P {
  borrowing func getDefault() -> dependsOn(self) E {
    return getE()
  }
}

public struct View: ~Escapable {
  // TODO: dependsOn(immortal)
  @_unsafeNonescapableResult
  init() { }
}

public struct PView: P {
  borrowing func getE() -> dependsOn(self) View { return View() }
}

public func test(pview: borrowing PView) -> dependsOn(pview) View {
  return pview.getDefault()
}

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics1PPAAE10getDefault1EQzYlsS_yF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> _scope(0)  @out Self.E {

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics5PViewV4getEAA4ViewVYlsS_yF : $@convention(method) (PView) -> _scope(0)  @owned View {

// CHECK-LABEL: sil private [transparent] [thunk] @$s28lifetime_dependence_generics5PViewVAA1PA2aDP4getE1EQzYlsS_yFTW : $@convention(witness_method: P) (@in_guaranteed PView) -> _scope(0)  @out View {

// CHECK-LABEL: sil @$s28lifetime_dependence_generics4test5pviewAA4ViewVYlsS_AA5PViewV_tF : $@convention(thin) (PView) -> _scope(0)  @owned View {

// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN: | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_SuppressedAssociatedTypes

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
  @lifetime(immortal)
  init() { }
}

public struct PView: P {
  @lifetime(immortal)
  borrowing func getE() -> View { return View() }
}

public func test(pview: borrowing PView) -> View {
  return pview.getDefault()
}

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics1PPAAE10getDefault1EQzyF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @lifetime(borrow address_for_deps 0)  @out Self.E {

// CHECK-LABEL: sil hidden @$s28lifetime_dependence_generics5PViewV4getEAA4ViewVyF : $@convention(method) (PView) -> @lifetime(immortal) @owned View {

// CHECK-LABEL: sil private [transparent] [thunk] @$s28lifetime_dependence_generics5PViewVAA1PA2aDP4getE1EQzyFTW : $@convention(witness_method: P) (@in_guaranteed PView) -> @lifetime(borrow 0)  @out View {

// CHECK-LABEL: sil @$s28lifetime_dependence_generics4test5pviewAA4ViewVAA5PViewV_tF : $@convention(thin) (PView) -> @lifetime(borrow 0)  @owned View {

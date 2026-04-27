// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

struct NE : ~Escapable {
    func condition() -> Bool {
        return true
    }
}

func takePicker(picker: @_lifetime(copy ne0, copy ne1) (_ ne0: NE, _ ne1: NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}

func callTakePicker(cond: Bool, ne: NE) {
  takePicker { ne0, ne1 in ne0 }
  takePicker { if cond { return $0 } else { return $1 } }
  takePicker { if ne.condition() { return $0 } else { return $1 } }
  takePicker { if cond || ne.condition() { return $0 } else { return $1 } }
}

// Closures in callTakePicker

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU_ : $@convention(thin) (@guaranteed NE, @guaranteed NE) -> @lifetime(copy 0, copy 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU_'

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU0_ : $@convention(thin) (@guaranteed NE, @guaranteed NE, Bool) -> @lifetime(copy 0, copy 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU0_'

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU1_ : $@convention(thin) (@guaranteed NE, @guaranteed NE, @guaranteed NE) -> @lifetime(copy 0, copy 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU1_'

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU2_ : $@convention(thin) (@guaranteed NE, @guaranteed NE, Bool, @guaranteed NE) -> @lifetime(copy 0, copy 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence14callTakePicker4cond2neySb_AA2NEVtFA2F_AFtXEfU2_'

func takeOnePicker(picker: @_lifetime(copy ne0) (_ ne0: NE, NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}

func takeOtherOnePicker(picker: @_lifetime(copy ne1) (NE, _ ne1: NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}

func callTakeOnePicker() {
  takeOnePicker { ne0, ne1 in ne0 }
  takeOtherOnePicker { ne0, ne1 in ne1 }
}

// Closures in callTakeOnePicker

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence17callTakeOnePickeryyFAA2NEVAD_ADtXEfU_ : $@convention(thin) (@guaranteed NE, @guaranteed NE) -> @lifetime(copy 0) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence17callTakeOnePickeryyFAA2NEVAD_ADtXEfU_'

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence17callTakeOnePickeryyFAA2NEVAD_ADtXEfU0_ : $@convention(thin) (@guaranteed NE, @guaranteed NE) -> @lifetime(copy 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence17callTakeOnePickeryyFAA2NEVAD_ADtXEfU0_'

// Closure Context Dependencies

// If there is another matching parameter to depend on, do not infer dependence on the context.
// CHECK-LABEL: sil hidden @$s27closure_lifetime_dependence22inferredLifetimePicker6pickeryAA2NEVAE_AEtXE_tF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (@guaranteed NE, @guaranteed NE) -> @lifetime(captures, copy 0, copy 1) @owned NE) -> () {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence22inferredLifetimePicker6pickeryAA2NEVAE_AEtXE_tF'
func inferredLifetimePicker(picker: (NE, NE) -> NE) {
}

// CHECK-LABEL: sil hidden @$s27closure_lifetime_dependence22contextDependentPicker6pickeryAA2NEVyXE_tF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @lifetime(captures) @owned NE) -> () {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence22contextDependentPicker6pickeryAA2NEVyXE_tF'
func contextDependentPicker(picker: () -> NE) {
  _ = picker()
}

func callContextDependentPicker() {
  let ne = NE()
  contextDependentPicker { ne }
}

// CHECK-LABEL: sil hidden @$s27closure_lifetime_dependence28contextAndArgDependentPicker6pickeryAA2NEVAEXE_tF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(captures, copy 0) @owned NE) -> () {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence28contextAndArgDependentPicker6pickeryAA2NEVAEXE_tF'
func contextAndArgDependentPicker(picker: @_lifetime(captures, copy ne) (_ ne: NE) -> NE) {
  let x = NE()
  _ = picker(x)
}

func callContextAndArgDependentPicker() {
  let ne = NE()
  contextAndArgDependentPicker { neArg in ne } // OK
  contextAndArgDependentPicker { neArg in neArg } // OK
}

// CHECK-LABEL: sil private @$s27closure_lifetime_dependence32callContextAndArgDependentPickeryyFAA2NEVADXEfU_ : $@convention(thin) (@guaranteed NE, @guaranteed NE) -> @lifetime(copy 0, borrow 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence32callContextAndArgDependentPickeryyFAA2NEVADXEfU_'

// The captures dependence here is technically unnecessary, but causes no harm, since the closure is only called in one place.
// CHECK-LABEL: sil private @$s27closure_lifetime_dependence32callContextAndArgDependentPickeryyFAA2NEVADXEfU0_ : $@convention(thin) (@guaranteed NE) -> @lifetime(captures, copy 0) @owned NE {
// CHECK-LABEL: } // end sil function '$s27closure_lifetime_dependence32callContextAndArgDependentPickeryyFAA2NEVADXEfU0_'

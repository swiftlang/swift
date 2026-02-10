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

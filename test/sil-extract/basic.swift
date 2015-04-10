// Passing demangled name

// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="basic.foo" | FileCheck %s -check-prefix=EXTRACT-FOO
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="basic.X.test" | FileCheck %s -check-prefix=EXTRACT-TEST
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="basic.Vehicle.init" | FileCheck %s -check-prefix=EXTRACT-INIT
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="basic.Vehicle.now" | FileCheck %s -check-prefix=EXTRACT-NOW

// Passing mangled name

// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="_TF5basic3fooFT_Si" | FileCheck %s -check-prefix=EXTRACT-FOO
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="_TFV5basic1X4testfS0_FT_T_" | FileCheck %s -check-prefix=EXTRACT-TEST
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="_TFC5basic7VehiclecfMS0_FT1nSi_S0_" | FileCheck %s -check-prefix=EXTRACT-INIT
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-extract -module-name basic -func="_TFC5basic7Vehicle3nowfS0_FT_Si" | FileCheck %s -check-prefix=EXTRACT-NOW


// EXTRACT-FOO-NOT: sil hidden @_TFV5basic1X4testfS0_FT_T_ : $@cc(method) @thin (X) -> () {
// EXTRACT-FOO-NOT: sil hidden @_TFC5basic7VehiclecfMS0_FT1nSi_S0_ : $@cc(method) @thin (Int, @owned Vehicle) -> @owned Vehicle {
// EXTRACT-FOO-NOT: sil hidden @_TFC5basic7Vehicle3nowfS0_FT_Si : $@cc(method) @thin (@owned Vehicle) -> Int {

// EXTRACT-FOO-LABEL: sil hidden @_TF5basic3fooFT_Si : $@thin () -> Int {
// EXTRACT-FOO:       bb0:
// EXTRACT-FOO-NEXT:    %0 = integer_literal
// EXTRACT-FOO-NEXT:    %1 = struct $Int
// EXTRACT-FOO-NEXT:    return %1 : $Int


// EXTRACT-TEST-NOT: sil hidden @_TF5basic3fooFT_Si : $@thin () -> Int {
// EXTRACT-TEST-NOT: sil hidden @_TFC5basic7VehiclecfMS0_FT1nSi_S0_ : $@cc(method) @thin (Int, @owned Vehicle) -> @owned Vehicle {
// EXTRACT-TEST-NOT: sil hidden @_TFC5basic7Vehicle3nowfS0_FT_Si : $@cc(method) @thin (@owned Vehicle) -> Int {

// EXTRACT-TEST-LABEL:  sil hidden @_TFV5basic1X4testfS0_FT_T_ : $@cc(method) @thin (X) -> () {
// EXTRACT-TEST:        bb0(%0 : $X):
// EXTRACT-TEST-NEXT:     debug_value
// EXTRACT-TEST-NEXT:     function_ref
// EXTRACT-TEST-NEXT:     function_ref @_TF5basic3fooFT_Si : $@thin () -> Int
// EXTRACT-TEST-NEXT:     apply
// EXTRACT-TEST-NEXT:     tuple
// EXTRACT-TEST-NEXT:     return


// EXTRACT-INIT-NOT: sil hidden @_TF5basic3fooFT_Si : $@thin () -> Int {
// EXTRACT-INIT-NOT: sil hidden @_TFV5basic1X4testfS0_FT_T_ : $@cc(method) @thin (X) -> () {
// EXTRACT-INIT-NOT: sil hidden @_TFC5basic7Vehicle3nowfS0_FT_Si : $@cc(method) @thin (@owned Vehicle) -> Int {

// EXTRACT-INIT-LABEL:   sil hidden @_TFC5basic7VehiclecfMS0_FT1nSi_S0_ : $@cc(method) @thin (Int, @owned Vehicle) -> @owned Vehicle {
// EXTRACT-INIT:         bb0
// EXTRACT-INIT-NEXT:      debug_value
// EXTRACT-INIT-NEXT:      debug_value
// EXTRACT-INIT-NEXT:      ref_element_addr
// EXTRACT-INIT-NEXT:      store
// EXTRACT-INIT-NEXT:      return


// EXTRACT-NOW-NOT: sil hidden @_TF5basic3fooFT_Si : $@thin () -> Int {
// EXTRACT-NOW-NOT: sil hidden @_TFV5basic1X4testfS0_FT_T_ : $@cc(method) @thin (X) -> () {
// EXTRACT-NOW-NOT: sil hidden @_TFC5basic7VehiclecfMS0_FT1nSi_S0_ : $@cc(method) @thin (Int, @owned Vehicle) -> @owned Vehicle {

// EXTRACT-NOW-LABEL:   sil hidden @_TFC5basic7Vehicle3nowfS0_FT_Si : $@cc(method) @thin (@owned Vehicle) -> Int {
// EXTRACT-NOW:         bb0
// EXTRACT-NOW-NEXT:      debug_value
// EXTRACT-NOW-NEXT:      strong_retain
// EXTRACT-NOW-NEXT:      ref_element_addr
// EXTRACT-NOW-NEXT:      load
// EXTRACT-NOW-NEXT:      strong_release
// EXTRACT-NOW-NEXT:      strong_release
// EXTRACT-NOW-NEXT:      return

struct X {
  func test() {
    foo()
  }
}

class Vehicle {
    var num_of_wheels: Int

    init(n: Int) {
      num_of_wheels = n
    }

    func now() -> Int {
        return num_of_wheels;
    }
}

func foo() -> Int {
  return 7;
}

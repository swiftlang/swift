// Passing demangled name

// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="basic.foo" | %FileCheck %s -check-prefix=EXTRACT-FOO
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="basic.X.test" | %FileCheck %s -check-prefix=EXTRACT-TEST
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="basic.Vehicle.init" | %FileCheck %s -check-prefix=EXTRACT-INIT
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="basic.Vehicle.now" | %FileCheck %s -check-prefix=EXTRACT-NOW

// Passing mangled name

// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="_TF5basic3fooFT_Si" | %FileCheck %s -check-prefix=EXTRACT-FOO
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="_TFV5basic1X4testfT_T_" | %FileCheck %s -check-prefix=EXTRACT-TEST
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="_TFC5basic7VehiclecfT1nSi_S0_" | %FileCheck %s -check-prefix=EXTRACT-INIT
// RUN: %target-swift-frontend %s -g -module-name basic -emit-sib -o - | %target-sil-func-extractor -module-name basic -func="_TFC5basic7Vehicle3nowfT_Si" | %FileCheck %s -check-prefix=EXTRACT-NOW


// EXTRACT-FOO-NOT: sil hidden @_TFV5basic1X4testfT_T_ : $@convention(method) (X) -> () {
// EXTRACT-FOO-NOT: sil hidden @_TFC5basic7VehiclecfT1nSi_S0_ : $@convention(method) (Int, @guaranteed Vehicle) -> @owned Vehicle {
// EXTRACT-FOO-NOT: sil hidden @_TFC5basic7Vehicle3nowfT_Si : $@convention(method) (@guaranteed Vehicle) -> Int {

// EXTRACT-FOO-LABEL: sil hidden @_TF5basic3fooFT_Si : $@convention(thin) () -> Int {
// EXTRACT-FOO:       bb0:
// EXTRACT-FOO-NEXT:    %0 = integer_literal
// EXTRACT-FOO-NEXT:    %1 = struct $Int
// EXTRACT-FOO-NEXT:    return %1 : $Int


// EXTRACT-TEST-NOT: sil hidden @_TF5basic3fooFT_Si : $@convention(thin) () -> Int {
// EXTRACT-TEST-NOT: sil hidden @_TFC5basic7VehiclecfT1nSi_S0_ : $@convention(method) (Int, @guaranteed Vehicle) -> @owned Vehicle {
// EXTRACT-TEST-NOT: sil hidden @_TFC5basic7Vehicle3nowfT_Si : $@convention(method) (@guaranteed Vehicle) -> Int {

// EXTRACT-TEST-LABEL:  sil hidden @_TFV5basic1X4testfT_T_ : $@convention(method) (X) -> () {
// EXTRACT-TEST:        bb0(%0 : $X):
// EXTRACT-TEST-NEXT:     function_ref
// EXTRACT-TEST-NEXT:     function_ref @_TF5basic3fooFT_Si : $@convention(thin) () -> Int
// EXTRACT-TEST-NEXT:     apply
// EXTRACT-TEST-NEXT:     tuple
// EXTRACT-TEST-NEXT:     return


// EXTRACT-INIT-NOT: sil hidden @_TF5basic3fooFT_Si : $@convention(thin) () -> Int {
// EXTRACT-INIT-NOT: sil hidden @_TFV5basic1X4testfT_T_ : $@convention(method) (X) -> () {
// EXTRACT-INIT-NOT: sil hidden @_TFC5basic7Vehicle3nowfT_Si : $@convention(method) (@owned Vehicle) -> Int {

// EXTRACT-INIT-LABEL:   sil hidden @_TFC5basic7VehiclecfT1nSi_S0_ : $@convention(method) (Int, @owned Vehicle) -> @owned Vehicle {
// EXTRACT-INIT:         bb0
// EXTRACT-INIT-NEXT:      ref_element_addr
// EXTRACT-INIT-NEXT:      store
// EXTRACT-INIT-NEXT:      return


// EXTRACT-NOW-NOT: sil hidden @_TF5basic3fooFT_Si : $@convention(thin) () -> Int {
// EXTRACT-NOW-NOT: sil hidden @_TFV5basic1X4testfT_T_ : $@convention(method) (X) -> () {
// EXTRACT-NOW-NOT: sil hidden @_TFC5basic7VehiclecfT1nSi_S0_ : $@convention(method) (Int, @guaranteed Vehicle) -> @owned Vehicle {

// EXTRACT-NOW-LABEL:   sil hidden @_TFC5basic7Vehicle3nowfT_Si : $@convention(method) (@guaranteed Vehicle) -> Int {
// EXTRACT-NOW:         bb0
// EXTRACT-NOW:           ref_element_addr
// EXTRACT-NOW-NEXT:      load
// EXTRACT-NOW-NEXT:      return

struct X {
  func test() {
    foo()
  }
}

class Vehicle {
    var numOfWheels: Int

    init(n: Int) {
      numOfWheels = n
    }

    func now() -> Int {
        return numOfWheels
    }
}

func foo() -> Int {
  return 7
}

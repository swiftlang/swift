// RUN: rm -rf %t && mkdir %t

// RUN: %swift %s -emit-silgen -module-name mangling_local | FileCheck %s

// Single nesting

// CHECK-LABEL: sil @_TF14mangling_local17singleNestingFuncFT_T_
public func singleNestingFunc() {
  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1S3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1SCfMS0_FT6singleSi_S0_
  struct S {
    let single: Int
    func foo() {}
  }
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1C3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1CD
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1Cd
  // CHECK-LABEL: sil shared [transparent] @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1Cg6singleSi
  class C {
    let single = 2
    func foo() {}
  }

  // CHECK-LABEL: sil shared [transparent] @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1E6SinglefMS0_FSiS0_
  // CHECK-LABEL: sil shared @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_1E3foofS0_FT_T_
  enum E {
    case Single(Int)
    func foo() {}
  }
}

// CHECK-LABEL: sil @_TF14mangling_local18singleNestingFunc1FT_T_
public func singleNestingFunc1() {
  // Identically-named nominal types in this function
  // should not collide with those in globalfunc() - they should be
  // uniqued with a file-level counter.

  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1S3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1SCfMS0_FT12singlesingleSi_S0_
  struct S {
    let singlesingle: Int
    func foo() {}
  }

  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1C3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1CcfMS0_FT_S0_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1CCfMS0_FT_S0_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1CD
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1Cd
  // CHECK-LABEL: sil shared [transparent] @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1Cg12singlesingleSi
  class C {
    let singlesingle = 2
    func foo() {}
  }

  // CHECK-LABEL: sil shared [transparent] @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1E12SingleSinglefMS0_FSiS0_
  // CHECK-LABEL: sil shared @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB0_1E3foofS0_FT_T_
  enum E {
    case SingleSingle(Int)
    func foo() {}
  }
}

// CHECK-LABEL: sil shared @_TF14mangling_localU_FT_T_
public let singleNestingClosure: () -> () = {
  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1S3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1SCfMS0_FT13singleclosureSi_S0_
  struct S {
    let singleclosure: Int
    func foo() {}
  }

  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1C3foofS0_FT_T_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1CcfMS0_FT_S0_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1CCfMS0_FT_S0_
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1CD
  // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1Cd
  // CHECK-LABEL: sil shared [transparent] @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1Cg13singleclosureSi
  class C {
    let singleclosure = 2
    func foo() {}
  }

  // CHECK-LABEL: sil shared [transparent] @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1E13SingleClosurefMS0_FSiS0_
  // CHECK-LABEL: sil shared @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB1_1E3foofS0_FT_T_
  enum E {
    case SingleClosure(Int)
    func foo() {}
  }
}

// Double nesting

// CHECK-LABEL: sil @_TF14mangling_local17doubleNestingFuncFT_T_
public func doubleNestingFunc() {
  // CHECK-LABEL: sil shared @_TFF14mangling_local17doubleNestingFuncFT_T_L33_E1597FE6B3964EFB379468CF6E89F9BB_5innerFT_T_
  func inner() {
    // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1S3foofS0_FT_T_
    // CHECK-LABEL:  sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1SCfMS0_FT6doubleSi_S0_
    struct S {
      let double: Int
      func foo() {}
    }

    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1C3foofS0_FT_T_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1CcfMS0_FT_S0_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1CCfMS0_FT_S0_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1CD
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1Cd
    // CHECK-LABEL: sil shared [transparent] @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1Cg6doubleSi
    class C {
      let double = 2
      func foo() {}
    }
    // CHECK-LABEL: sil shared [transparent] @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1E6DoublefMS0_FSiS0_
    // CHECK-LABEL: sil shared @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB2_1E3foofS0_FT_T_
    enum E {
      case Double(Int)
      func foo() {}
    }
  }
  inner()
}

public let doubleNestingClosure: () -> () = {
  let inner: () -> () = {
    // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1S3foofS0_FT_T_
    // CHECK-LABEL: sil shared @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1SCfMS0_FT13doubleclosureSi_S0_
    struct S {
      let doubleclosure: Int
      func foo() {}
    }

    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1C3foofS0_FT_T_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1CcfMS0_FT_S0_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1CCfMS0_FT_S0_
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1CD
    // CHECK-LABEL: sil shared @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1Cd
    // CHECK-LABEL: sil shared [transparent] @_TFC14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1Cg13doubleclosureSi
    class C {
      let doubleclosure = 2
      func foo() {}
    }

    // CHECK-LABEL: sil shared [transparent] @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1E13DoubleClosurefMS0_FSiS0_
    // CHECK-LABEL: sil shared @_TFO14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB3_1E3foofS0_FT_T_
    enum E {
      case DoubleClosure(Int)
      func foo() {}
    }
  }
  inner()
}

// With transparent

// CHECK-LABEL: sil hidden @_TF14mangling_local19IncludesTransparentFT_T_
func IncludesTransparent() {
  // CHECK-LABEL: sil shared [transparent] @_TFF14mangling_local19IncludesTransparentFT_T_L33_E1597FE6B3964EFB379468CF6E89F9BB_15WithTransparentFT_T_
  @transparent func WithTransparent() {
    // CHECK-LABEL: sil @_TFV14mangling_localL33_E1597FE6B3964EFB379468CF6E89F9BB_13InTransparentCfMS0_FT11transparentSS_S0_
    struct InTransparent {
      let transparent: String
    }
    println(InTransparent(transparent: "Hello from transparent!").transparent)
  }
  WithTransparent()
}

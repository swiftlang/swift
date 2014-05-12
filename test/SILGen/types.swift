// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

class C {
  var member: Int = 0

  // Methods have method calling convention.
  // CHECK-LABEL: sil  @{{.*}}C3foo{{.*}} : $@cc(method) @thin (Int, @owned C) -> ()
  func foo(`x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $C):
    member = x

    // CHECK: strong_retain %1 : $C
    // CHECK: [[FN:%[0-9]+]] = class_method %1 : $C, #C.member!setter.1
    // CHECK: apply [[FN]](%0, %1) : $@cc(method) @thin (Int, @owned C) -> ()
    // CHECK: strong_release %1 : $C


  }
}

struct S {
  var member: Int

  // CHECK-LABEL: sil  @{{.*}}foo{{.*}} : $@cc(method) @thin (Int, @inout S) -> ()
  mutating
  func foo(var `x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $*S):
    member = x
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[THIS_LOCAL:%[0-9]+]] = alloc_box $S
    // CHECK: [[MEMBER:%[0-9]+]] = struct_element_addr [[THIS_LOCAL]]#1 : $*S, #S.member
    // CHECK: copy_addr [[XADDR]]#1 to [[MEMBER]]
  }

  class SC {
    // CHECK-LABEL: sil  @_TFCV5types1S2SC3barfS1_FT_T_
    func bar() {}
  }
}

func f() {
  class FC {
    // CHECK-LABEL: sil shared @_TFCF5types1fFT_T_L_2FC3zimfS0_FT_T_
    func zim() {}
  }
}

func g(`b : Bool) {
  if (b) {
    class FC {
      // CHECK-LABEL: sil shared @_TFCF5types1gFT1bSb_T_L_2FC3zimfS0_FT_T_
      func zim() {}
    }
  } else {
    class FC {
      // CHECK-LABEL: sil shared @_TFCF5types1gFT1bSb_T_L0_2FC3zimfS0_FT_T_
      func zim() {}
    }
  }
}

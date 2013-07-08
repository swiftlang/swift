// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

class C {
  var member : Int

  // Methods have method calling convention.
  // CHECK: sil @_TC5types1C3foofS0_FT1xSi_T_ : $[cc(method), thin] ((x : Int64), C) -> ()
  func foo(x:Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int64, [[THIS:%[0-9]+]] : $C):
    member = x
    // CHECK: [[XADDR:%[0-9]+]] = alloc_var stack $Int64
    // CHECK: [[THISADDR:%[0-9]+]] = alloc_var stack $C
    // CHECK: [[X1:%[0-9]+]] = load [[XADDR]]
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISADDR]]
    // CHECK: [[MEMBER:%[0-9]+]] = ref_element_addr [[THIS1]], @member
    // CHECK: store [[X1]] to [[MEMBER]]
  }
}

struct S {
  var member : Int

  // CHECK: sil @_TV5types1S3foofRS0_FT1xSi_T_ : $[cc(method), thin] ((x : Int64), [byref] S) -> () 
  func foo(x:Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int64, [[THIS:%[0-9]+]] : $*S):
    member = x
    // CHECK: [[XADDR:%[0-9]+]] = alloc_var stack $Int64
    // CHECK: [[X1:%[0-9]+]] = load [[XADDR]]
    // CHECK: [[MEMBER:%[0-9]+]] = struct_element_addr [[THIS]], @member
    // CHECK: store [[X1]] to [[MEMBER]]
  }

  class SC {
    // CHECK: sil @_TCV5types1S2SC3barfS1_FT_T_
    func bar() {}
  }
}

func f() {
  class FC {
    // CHECK: sil internal @_TLC5types1fFT_T_2FC3zimfS0_FT_T_
    func zim() {}
  }
}

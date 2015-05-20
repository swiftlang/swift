// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s

class C {
  var member: Int = 0

  // Methods have method calling convention.
  // CHECK-LABEL: sil hidden @_TFC5types1C3foofS0_FT1xSi_T_ : $@convention(method) (Int, @guaranteed C) -> () {
  func foo(x x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $C):
    member = x

    // CHECK-NOT: strong_retain
    // CHECK: [[FN:%[0-9]+]] = class_method %1 : $C, #C.member!setter.1
    // CHECK: apply [[FN]](%0, %1) : $@convention(method) (Int, @guaranteed C) -> ()
    // CHECK-NOT: strong_release


  }
}

struct S {
  var member: Int

  // CHECK-LABEL: sil hidden  @{{.*}}foo{{.*}} : $@convention(method) (Int, @inout S) -> ()
  mutating
  func foo(var x x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $*S):
    member = x
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
    // CHECK: [[THIS_LOCAL:%[0-9]+]] = alloc_box $S
    // CHECK: [[MEMBER:%[0-9]+]] = struct_element_addr [[THIS_LOCAL]]#1 : $*S, #S.member
    // CHECK: copy_addr [[XADDR]]#1 to [[MEMBER]]
  }

  class SC {
    // CHECK-LABEL: sil hidden  @_TFCV5types1S2SC3barfS1_FT_T_
    func bar() {}
  }
}

func f() {
  class FC {
    // CHECK-LABEL: sil shared @_TFCF5types1fFT_T_L_2FC3zimfS0_FT_T_
    func zim() {}
  }
}

func g(b b : Bool) {
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

struct ReferencedFromFunctionStruct {
  let f: ReferencedFromFunctionStruct -> () = {x in ()}
  let g: ReferencedFromFunctionEnum -> () = {x in ()}
}

enum ReferencedFromFunctionEnum {
  case f(ReferencedFromFunctionEnum -> ())
  case g(ReferencedFromFunctionStruct -> ())
}

// CHECK-LABEL: sil hidden @_TF5types34referencedFromFunctionStructFieldsFVS_28ReferencedFromFunctionStructTFS0_T_FOS_26ReferencedFromFunctionEnumT__
// CHECK:         [[F:%.*]] = struct_extract [[X:%.*]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.f
// CHECK:         [[F]] : $@callee_owned (@owned ReferencedFromFunctionStruct) -> ()
// CHECK:         [[G:%.*]] = struct_extract [[X]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.g
// CHECK:         [[G]] : $@callee_owned (@owned ReferencedFromFunctionEnum) -> ()
func referencedFromFunctionStructFields(x: ReferencedFromFunctionStruct)
    -> (ReferencedFromFunctionStruct -> (), ReferencedFromFunctionEnum -> ()) {
  return (x.f, x.g)
}

// CHECK-LABEL: sil hidden @_TF5types32referencedFromFunctionEnumFieldsFOS_26ReferencedFromFunctionEnumTGSqFS0_T__GSqFVS_28ReferencedFromFunctionStructT___
// CHECK:       bb{{[0-9]+}}([[F:%.*]] : $@callee_owned (@owned ReferencedFromFunctionEnum) -> ()):
// CHECK:       bb{{[0-9]+}}([[G:%.*]] : $@callee_owned (@owned ReferencedFromFunctionStruct) -> ()):
func referencedFromFunctionEnumFields(x: ReferencedFromFunctionEnum)
    -> (
      (ReferencedFromFunctionEnum -> ())?,
      (ReferencedFromFunctionStruct -> ())?
    ) {
  switch x {
  case .f(let f):
    return (f, nil)
  case .g(let g):
    return (nil, g)
  }
}

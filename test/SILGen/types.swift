
// RUN: %target-swift-emit-silgen -module-name types -parse-as-library -enable-sil-ownership %s | %FileCheck %s

class C {
  var member: Int = 0

  // Methods have method calling convention.
  // CHECK-LABEL: sil hidden @$s5types1CC3foo1xySi_tF : $@convention(method) (Int, @guaranteed C) -> () {
  func foo(x x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[THIS:%[0-9]+]] : @guaranteed $C):
    member = x

    // CHECK-NOT: copy_value
    // CHECK: [[FN:%[0-9]+]] = class_method %1 : $C, #C.member!setter.1
    // CHECK: apply [[FN]](%0, %1) : $@convention(method) (Int, @guaranteed C) -> ()
    // CHECK-NOT: destroy_value
  }
  // CHECK: } // end sil function '$s5types1CC3foo1xySi_tF'
}

struct S {
  var member: Int

  // CHECK-LABEL: sil hidden @{{.*}}1SV3foo{{.*}} : $@convention(method) (Int, @inout S) -> ()
  mutating
  func foo(x x: Int) {
    var x = x
    // CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[THIS:%[0-9]+]] : @trivial $*S):
    member = x
    // CHECK: [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
    // CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
    // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[XADDR]] : $*Int
    // CHECK: [[X:%.*]] = load [trivial] [[READ]] : $*Int
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[THIS]] : $*S
    // CHECK: [[MEMBER:%[0-9]+]] = struct_element_addr [[WRITE]] : $*S, #S.member
    // CHECK: assign [[X]] to [[MEMBER]] : $*Int
  }

  class SC {
    // CHECK-LABEL: sil hidden @$s5types1SV2SCC3bar{{.*}}
    func bar() {}
  }
}

func f() {
  class FC {
    func zim() {}
  }
}

func g(b b : Bool) {
  if (b) {
    class FC {
      func zim() {}
    }
  } else {
    class FC {
      func zim() {}
    }
  }
}

struct ReferencedFromFunctionStruct {
  let f: (ReferencedFromFunctionStruct) -> () = {x in ()}
  let g: (ReferencedFromFunctionEnum) -> () = {x in ()}
}

enum ReferencedFromFunctionEnum {
  case f((ReferencedFromFunctionEnum) -> ())
  case g((ReferencedFromFunctionStruct) -> ())
}

// CHECK-LABEL: sil hidden @$s5types34referencedFromFunctionStructFieldsyyAA010ReferencedcdE0Vc_yAA0gcD4EnumOctADF{{.*}} : $@convention(thin) (@guaranteed ReferencedFromFunctionStruct) -> (@owned @callee_guaranteed (@guaranteed ReferencedFromFunctionStruct) -> (), @owned @callee_guaranteed (@guaranteed ReferencedFromFunctionEnum) -> ()) {
// CHECK: bb0([[X:%.*]] : @guaranteed $ReferencedFromFunctionStruct):
// CHECK:   [[F:%.*]] = struct_extract [[X]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.f
// CHECK:   [[COPIED_F:%.*]] = copy_value [[F]] : $@callee_guaranteed (@guaranteed ReferencedFromFunctionStruct) -> ()
// CHECK:   [[G:%.*]] = struct_extract [[X]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.g
// CHECK:   [[COPIED_G:%.*]] = copy_value [[G]] : $@callee_guaranteed (@guaranteed ReferencedFromFunctionEnum) -> ()
// CHECK:   [[RESULT:%.*]] = tuple ([[COPIED_F]] : {{.*}}, [[COPIED_G]] : {{.*}})
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s5types34referencedFromFunctionStructFieldsyyAA010ReferencedcdE0Vc_yAA0gcD4EnumOctADF'
func referencedFromFunctionStructFields(_ x: ReferencedFromFunctionStruct)
    -> ((ReferencedFromFunctionStruct) -> (), (ReferencedFromFunctionEnum) -> ()) {
  return (x.f, x.g)
}

// CHECK-LABEL: sil hidden @$s5types32referencedFromFunctionEnumFieldsyyAA010ReferencedcdE0OcSg_yAA0gcD6StructVcSgtADF
// CHECK:       bb{{[0-9]+}}([[F:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed ReferencedFromFunctionEnum) -> ()):
// CHECK:       bb{{[0-9]+}}([[G:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed ReferencedFromFunctionStruct) -> ()):
func referencedFromFunctionEnumFields(_ x: ReferencedFromFunctionEnum)
    -> (
      ((ReferencedFromFunctionEnum) -> ())?,
      ((ReferencedFromFunctionStruct) -> ())?
    ) {
  switch x {
  case .f(let f):
    return (f, nil)
  case .g(let g):
    return (nil, g)
  }
}

// CHECK-LABEL: sil private @$s5types1fyyF2FCL_C3zimyyF
// CHECK-LABEL: sil private @$s5types1g1bySb_tF2FCL_C3zimyyF
// CHECK-LABEL: sil private @$s5types1g1bySb_tF2FCL0_C3zimyyF

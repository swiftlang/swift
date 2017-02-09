// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-as-library -emit-silgen %s | %FileCheck %s

class C {
  var member: Int = 0

  // Methods have method calling convention.
  // CHECK-LABEL: sil hidden @_T05types1CC3fooySi1x_tF : $@convention(method) (Int, @guaranteed C) -> () {
  func foo(x x: Int) {
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $C):
    member = x

    // CHECK-NOT: copy_value
    // CHECK: [[FN:%[0-9]+]] = class_method %1 : $C, #C.member!setter.1
    // CHECK: apply [[FN]](%0, %1) : $@convention(method) (Int, @guaranteed C) -> ()
    // CHECK-NOT: destroy_value
  }
  // CHECK: } // end sil function '_T05types1CC3fooySi1x_tF'
}

struct S {
  var member: Int

  // CHECK-LABEL: sil hidden  @{{.*}}1SV3foo{{.*}} : $@convention(method) (Int, @inout S) -> ()
  mutating
  func foo(x x: Int) {
    var x = x
    // CHECK: bb0([[X:%[0-9]+]] : $Int, [[THIS:%[0-9]+]] : $*S):
    member = x
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
    // CHECK: [[X:%[0-9]+]] = project_box [[XADDR]]
    // CHECK: [[MEMBER:%[0-9]+]] = struct_element_addr [[THIS]] : $*S, #S.member
    // CHECK: copy_addr [[X]] to [[MEMBER]]
  }

  class SC {
    // CHECK-LABEL: sil hidden @_T05types1SV2SCC3bar{{.*}}
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

// CHECK-LABEL: sil hidden @_T05types34referencedFromFunctionStructFieldsyAA010ReferencedcdE0Vc_yAA0gcD4EnumOctADF{{.*}} : $@convention(thin) (@owned ReferencedFromFunctionStruct) -> (@owned @callee_owned (@owned ReferencedFromFunctionStruct) -> (), @owned @callee_owned (@owned ReferencedFromFunctionEnum) -> ()) {
// CHECK: bb0([[X:%.*]] : $ReferencedFromFunctionStruct):
// CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
// CHECK:   [[F:%.*]] = struct_extract [[BORROWED_X]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.f
// CHECK:   [[COPIED_F:%.*]] = copy_value [[F]] : $@callee_owned (@owned ReferencedFromFunctionStruct) -> ()
// CHECK:   [[BORROWED_X_2:%.*]] = begin_borrow [[X]]
// CHECK:   [[G:%.*]] = struct_extract [[BORROWED_X_2]] : $ReferencedFromFunctionStruct, #ReferencedFromFunctionStruct.g
// CHECK:   [[COPIED_G:%.*]] = copy_value [[G]] : $@callee_owned (@owned ReferencedFromFunctionEnum) -> ()
// CHECK:   end_borrow [[BORROWED_X_2]] from [[X]]
// CHECK:   end_borrow [[BORROWED_X]] from [[X]]
// CHECK:   destroy_value [[X]]
// CHECK:   [[RESULT:%.*]] = tuple ([[COPIED_F]] : {{.*}}, [[COPIED_G]] : {{.*}})
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '_T05types34referencedFromFunctionStructFieldsyAA010ReferencedcdE0Vc_yAA0gcD4EnumOctADF'
func referencedFromFunctionStructFields(_ x: ReferencedFromFunctionStruct)
    -> ((ReferencedFromFunctionStruct) -> (), (ReferencedFromFunctionEnum) -> ()) {
  return (x.f, x.g)
}

// CHECK-LABEL: sil hidden @_T05types32referencedFromFunctionEnumFieldsyAA010ReferencedcdE0OcSg_yAA0gcD6StructVcSgtADF
// CHECK:       bb{{[0-9]+}}([[F:%.*]] : $@callee_owned (@owned ReferencedFromFunctionEnum) -> ()):
// CHECK:       bb{{[0-9]+}}([[G:%.*]] : $@callee_owned (@owned ReferencedFromFunctionStruct) -> ()):
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

// CHECK-LABEL: sil shared @_T05types1fyyF2FCL_C3zimyyF
// CHECK-LABEL: sil shared @_T05types1gySb1b_tF2FCL_C3zimyyF
// CHECK-LABEL: sil shared @_T05types1gySb1b_tF2FCL0_C3zimyyF

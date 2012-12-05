// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

func read_only_capture(x:Int) -> Int {
  // CHECK: func_decl cap
  func cap() -> Int {
    // CHECK: bb0([[XBOX:%[0-9]+]] : Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : [byref{{.*}}] Int):
    return x
    // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
    // CHECK: release [[XBOX]]
    // CHECK: return ([[X]])
  }

  // CHECK: func_decl read_only_capture
  // CHECK: bb0([[X:%[0-9]+]] : Int):
  return cap()
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[CAP:%[0-9]+]] = constant_ref ${{.*}}, @cap
  // XFAIL: [[CAP_CLOSURE:%[0-9]+]] = closure [[CAP]]([[XBOX]]#0, [[XBOX]]#1)
  // XFAIL: [[RET:%[0-9]+]] = apply [[CAP_CLOSURE]]()
  // CHECK: release [[XBOX]]#0
  // XFAIL: return ([[RET]])
}

func write_to_capture(x:Int) -> Int {
  // CHECK: func_decl scribble
  func scribble() {
    // CHECK: bb0([[XBOX:%[0-9]+]] : Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : [byref{{.*}}] Int):
    x = 1
    // CHECK: store {{%[0-9]+}} to [[XADDR]]
    // CHECK: release [[XBOX]]
    // CHECK: return
  }

  // CHECK: func_decl write_to_capture
  // CHECK: bb0([[X:%[0-9]+]] : Int):
  scribble()
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[SCRIB:%[0-9]+]] = constant_ref ${{.*}}, @scribble
  // XFAIL: [[SCRIB_CLOSURE:%[0-9]+]] = closure [[SCRIB]]([[XBOX]]#0, [[XBOX]]#1)
  // XFAIL: apply [[SCRIB_CLOSURE]]()
  // CHECK: [[RET:%[0-9]+]] = load [[XBOX]]#1
  // CHECK: release [[XBOX]]#0
  // XFAIL: return ([[RET]])
  return x
}

// TODO nested capture
// TODO byref downward capture
// TODO capture local func
// TODO anonymous function/explicit closure/implicit closure exprs

// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

// CHECK: sil @_T8closures17read_only_captureFT1xSi_Si
func read_only_capture(x:Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int64):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  func cap() -> Int {
    return x
  }
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: [[CAP_CLOSURE:%[0-9]+]] = partial_apply [[CAP]]([[XBOX]]#0, [[XBOX]]#1)

  return cap()
  // CHECK: retain [[CAP_CLOSURE]]
  // CHECK: [[RET:%[0-9]+]] = apply [[CAP_CLOSURE]]()
  // CHECK: release [[CAP_CLOSURE]]
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}

// CHECK: sil internal @[[CAP_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]

// CHECK: sil @_T8closures16write_to_captureFT1xSi_Si
func write_to_capture(x:Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int64):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  func scribble() {
    x = 1
  }
  // -- FIXME: [[SCRIB:%[0-9]+]] = function_ref @_TL8closures16write_to_captureFT1xSi_Si8scribblefT_T_
  // CHECK: [[SCRIB:%[0-9]+]] = function_ref @[[SCRIB_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> ()
  // CHECK: [[SCRIB_CLOSURE:%[0-9]+]] = partial_apply [[SCRIB]]([[XBOX]]#0, [[XBOX]]#1)

  scribble()
  // CHECK: retain [[SCRIB_CLOSURE]]
  // CHECK: apply [[SCRIB_CLOSURE]]()
  // CHECK: [[RET:%[0-9]+]] = load [[XBOX]]#1
  // CHECK: release [[SCRIB_CLOSURE]]
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
  return x
}

// CHECK: sil internal @[[SCRIB_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):
// CHECK: store {{%[0-9]+}} to [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return

// CHECK: sil @_T8closures21multiple_closure_refsFT1xSi_TFT_SiFT_Si_
func multiple_closure_refs(x:Int) -> (() -> Int, () -> Int) {
  func cap() -> Int {
    return x
  }
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: [[CAP_CLOSURE:%[0-9]+]] = partial_apply [[CAP]]

  return (cap, cap)
  // CHECK: retain [[CAP_CLOSURE]]
  // CHECK: retain [[CAP_CLOSURE]]
  // CHECK: [[RET:%[0-9]+]] = tuple ([[CAP_CLOSURE]] : {{.*}}, [[CAP_CLOSURE]] : {{.*}})
  // CHECK: release [[CAP_CLOSURE]]
  // CHECK: return [[RET]]
}

// CHECK: sil @_T8closures18capture_local_funcFT1xSi_FT_FT_Si
func capture_local_func(x:Int) -> () -> () -> Int {
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  func aleph() -> Int { return x }
  // CHECK: [[ALEPH_REF:%[0-9]+]] = function_ref @[[ALEPH_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: [[ALEPH_CLOSURE:%[0-9]+]] = partial_apply [[ALEPH_REF]]([[XBOX]]#0, [[XBOX]]#1)

  func beth() -> () -> Int { return aleph }
  // CHECK: [[BETH_REF:%[0-9]+]] = function_ref @[[BETH_NAME:closure[0-9]*]] : $[thin] ((), () -> Int64) -> () -> Int64
  // CHECK: [[BETH_CLOSURE:%[0-9]+]] = partial_apply [[BETH_REF]]([[ALEPH_CLOSURE]])

  return beth
  // CHECK: retain [[BETH_CLOSURE]]
  // CHECK: release [[BETH_CLOSURE]]
  // CHECK: release [[ALEPH_CLOSURE]]
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[BETH_CLOSURE]]
}
// CHECK: sil internal @[[ALEPH_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):

// CHECK: sil internal @[[BETH_NAME]]
// CHECK: bb0([[ALEPH:%[0-9]+]] : $() -> Int64):
// CHECK: retain [[ALEPH]]
// CHECK: release [[ALEPH]]
// CHECK: return [[ALEPH]]

// CHECK: sil @_T8closures22anon_read_only_captureFT1xSi_Si
func anon_read_only_capture(x:Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int64):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  return ({ x })()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON_CLOSURE]]()
  // -- cleanup
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}
// CHECK: sil internal @[[CLOSURE_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]

// CHECK: sil @_T8closures21small_closure_captureFT1xSi_Si
func small_closure_capture(x:Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int64):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  return { x }()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:closure[0-9]*]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON_CLOSURE]]()
  // -- cleanup
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}
// CHECK: sil internal @[[CLOSURE_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]


// CHECK: sil @_T8closures35small_closure_capture_with_argumentFT1xSi_FT1ySi_Si
func small_closure_capture_with_argument(x:Int) -> (y:Int) -> Int {
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int64

  return { x + $0 }
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:closure[0-9]*]] : $[thin] (($0 : Int64), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE_APP:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = convert_function [[ANON_CLOSURE_APP]] : ${{.*}} to $(y : Int64) -> Int64
  // -- return
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[ANON_CLOSURE]]
}
// CHECK: sil internal @[[CLOSURE_NAME]] : $[thin] (($0 : Int64), (Builtin.ObjectPointer, [byref] Int64)) -> Int64
// CHECK: bb0([[DOLLAR0:%[0-9]+]] : $Int64, [[XBOX:%[0-9]+]] : $Builtin.ObjectPointer, [[XADDR:%[0-9]+]] : $*Int64):
// CHECK: [[DOLLAR0ADDR:%[0-9]+]] = alloc_var stack $Int64
// CHECK: store [[DOLLAR0]] to [[DOLLAR0ADDR]]
// CHECK: [[PLUS:%[0-9]+]] = function_ref @_TSsoi1pFT3lhsSi3rhsSi_Si
// CHECK: [[LHS:%[0-9]+]] = load [[XADDR]]
// CHECK: [[RHS:%[0-9]+]] = load [[DOLLAR0ADDR]]
// CHECK: [[RET:%[0-9]+]] = apply [[PLUS]]([[LHS]], [[RHS]])
// CHECK: release [[XBOX]]
// CHECK: dealloc_var stack [[DOLLAR0ADDR]]
// CHECK: return [[RET]]

// CHECK: sil @_T8closures24small_closure_no_captureFT_FT1ySi_Si
func small_closure_no_capture() -> (y:Int) -> Int {
  // CHECK:   [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:closure[0-9]*]] : $[thin] ($0 : Int64) -> Int64
  // CHECK:   [[ANON_CONV:%[0-9]+]] = convert_function [[ANON]] : ${{.*}} to $[thin] (y : Int64) -> Int64
  // CHECK:   [[ANON_THICK:%[0-9]+]] = thin_to_thick_function [[ANON_CONV]] : ${{.*}} to $(y : Int64) -> Int64
  // CHECK:   return [[ANON_THICK]]
  return { $0 }
}
// CHECK: sil internal @[[CLOSURE_NAME]] : $[thin] ($0 : Int64) -> Int64
// CHECK: bb0([[YARG:%[0-9]+]] : $Int64):

// CHECK: sil @_T8closures17uncaptured_localsFT1xSi_TSiSi_ :
func uncaptured_locals(x:Int) -> (Int, Int) {
  // -- locals without captures are stack-allocated
  // CHECK: bb0([[XARG:%[0-9]+]] : $Int64):
  // CHECK:   [[XADDR:%[0-9]+]] = alloc_var stack $Int64
  // CHECK:   store [[XARG]] to [[XADDR]]

  var y = 1
  // CHECK:   [[YADDR:%[0-9]+]] = alloc_var stack $Int64
  return (x, y)

  // CHECK:   dealloc_var stack [[YADDR]]
  // CHECK:   dealloc_var stack [[XADDR]]
}

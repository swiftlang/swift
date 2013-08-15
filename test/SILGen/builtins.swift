// RUN: %swift -emit-silgen -parse-stdlib %s | FileCheck %s

// CHECK: sil @_T8builtins3fooFT1xBi1_1yBi1__Bi1_
func foo(x:Builtin.Int1, y:Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin_function_ref #Builtin.cmp_eq_Int1
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK: sil @_T8builtins8load_podFT1xBp_Bi64_
func load_pod(x:Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK: sil @_T8builtins8load_objFT1xBp_Bo
func load_obj(x:Builtin.RawPointer) -> Builtin.ObjectPointer {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.ObjectPointer
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK: sil @_T8builtins8load_genU__FT1xBp_Q_
func load_gen<T>(x:Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK: sil @_T8builtins8move_podFT1xBp_Bi64_
func move_pod(x:Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.move(x)
}

// CHECK: sil @_T8builtins8move_objFT1xBp_Bo
func move_obj(x:Builtin.RawPointer) -> Builtin.ObjectPointer {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.ObjectPointer
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK-NOT: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.move(x)
}

// CHECK: sil @_T8builtins8move_genU__FT1xBp_Q_
func move_gen<T>(x:Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.move(x)
}

// CHECK: sil @_T8builtins11destroy_podFT1xBp_T_
func destroy_pod(x:Builtin.RawPointer) {
  // CHECK: %1 = alloc_box
  // CHECK-NOT: pointer_to_address
  // CHECK-NOT: destroy_addr
  // CHECK-NOT: release
  // CHECK: release %1#0 : $Builtin.ObjectPointer
  // CHECK-NOT: release
  return Builtin.destroy(Builtin.Int64, x)
  // CHECK: return
}

// CHECK: sil @_T8builtins11destroy_objFT1xBp_T_
func destroy_obj(x:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.ObjectPointer
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.ObjectPointer, x)
}

// CHECK: sil @_T8builtins11destroy_genU__FT1xBp_T_
func destroy_gen<T>(x:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T, x)
}

// CHECK: sil @_T8builtins10assign_podFT1xBi64_1yBp_T_
func assign_pod(x:Builtin.Int64, y:Builtin.RawPointer) {
  // CHECK: alloc_box
  // CHECK: alloc_box
  // CHECK-NOT: alloc_box
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK: release
  // CHECK: release
  // CHECK-NOT: release
  Builtin.assign(x, y)
  // CHECK: return
}

// CHECK: sil @_T8builtins10assign_objFT1xBo1yBp_T_
func assign_obj(x:Builtin.ObjectPointer, y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.ObjectPointer
  // CHECK: [[OLD:%.*]] = load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK: release [[OLD]]
  Builtin.assign(x, y)
}

// CHECK: sil @_T8builtins12assign_tupleFT1xTBi64_Bo_1yBp_T_
func assign_tuple(x:(Builtin.Int64, Builtin.ObjectPointer),
                  y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*(Builtin.Int64, Builtin.ObjectPointer)
  // CHECK: [[OLD:%.*]] = load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK: [[OLD1:%.*]] = tuple_extract [[OLD]] : {{.*}}, 1
  // CHECK: release [[OLD1]]
  Builtin.assign(x, y)
}

// CHECK: sil @_T8builtins10assign_genU__FT1xQ_1yBp_T_
func assign_gen<T>(x:T, y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK: sil @_T8builtins8init_podFT1xBi64_1yBp_T_
func init_pod(x:Builtin.Int64, y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK-NOT: release [[ADDR]]
  Builtin.init(x, y)
}

// CHECK: sil @_T8builtins8init_objFT1xBo1yBp_T_
func init_obj(x:Builtin.ObjectPointer, y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.ObjectPointer
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [[ADDR]]
  // CHECK-NOT: release [[SRC]]
  Builtin.init(x, y)
}

// CHECK: sil @_T8builtins8init_genU__FT1xQ_1yBp_T_
func init_gen<T>(x:T, y:Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [initialization]  [[ADDR]]
  Builtin.init(x, y)
}

class C {}

// CHECK: sil @_T8builtins23class_to_object_pointerFT1cCS_1C_Bo
func class_to_object_pointer(c:C) -> Builtin.ObjectPointer {
  // CHECK: [[OBJ:%.*]] = ref_to_object_pointer [[C:%.*]] to $Builtin.ObjectPointer
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToObjectPointer(c)
}

func class_archetype_to_object_pointer<T:C>(t:T) -> Builtin.ObjectPointer {
  return Builtin.castToObjectPointer(t)
}

// CHECK: sil @_T8builtins25class_from_object_pointerFT1pBo_CS_1C
func class_from_object_pointer(p:Builtin.ObjectPointer) -> C {
  // CHECK: [[C:%.*]] = object_pointer_to_ref [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromObjectPointer(p)
}

func class_archetype_from_object_pointer<T:C>(p:Builtin.ObjectPointer) -> T {
  return Builtin.castFromObjectPointer(p)
}

// CHECK: sil @_T8builtins20class_to_raw_pointerFT1cCS_1C_Bp
func class_to_raw_pointer(c:C) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: release [[C]]
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

func class_archetype_to_raw_pointer<T:C>(t:T) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(t)
}

// CHECK: sil @_T8builtins18obj_to_raw_pointerFT1cBo_Bp
func obj_to_raw_pointer(c:Builtin.ObjectPointer) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: release [[C]]
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK: sil @_T8builtins22class_from_raw_pointerFT1pBp_CS_1C
func class_from_raw_pointer(p:Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T:C>(p:Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK: sil @_T8builtins20obj_from_raw_pointerFT1pBp_Bo
func obj_from_raw_pointer(p:Builtin.RawPointer) -> Builtin.ObjectPointer {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.ObjectPointer
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK: sil @_T8builtins5gep64FT1pBp1iBi64__Bp
func gep64(p:Builtin.RawPointer, i:Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int64(p, i)
}

// CHECK: sil @_T8builtins5gep32FT1pBp1iBi32__Bp
func gep32(p:Builtin.RawPointer, i:Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int32(p, i)
}

// RUN: %swift -emit-silgen -parse-stdlib %s | FileCheck %s
// RUN: %swift -emit-sil -O0 -parse-stdlib %s | FileCheck -check-prefix=CANONICAL %s

@class_protocol protocol ClassProto { }

// CHECK-LABEL: sil @_TF8builtins3foo
func foo(x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin_function_ref "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil @_TF8builtins8load_pod
func load_pod(x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil @_TF8builtins8load_obj
func load_obj(x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil @_TF8builtins8load_gen
func load_gen<T>(x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil @_TF8builtins8move_pod
func move_pod(x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil @_TF8builtins8move_obj
func move_obj(x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK-NOT: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil @_TF8builtins8move_gen
func move_gen<T>(x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil @_TF8builtins11destroy_pod
func destroy_pod(var x: Builtin.RawPointer) {
  // CHECK: %1 = alloc_box
  // CHECK-NOT: pointer_to_address
  // CHECK-NOT: destroy_addr
  // CHECK-NOT: release
  // CHECK: release %1#0 : $Builtin.NativeObject
  // CHECK-NOT: release
  return Builtin.destroy(Builtin.Int64, x)
  // CHECK: return
}

// CHECK-LABEL: sil @_TF8builtins11destroy_obj
func destroy_obj(x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject, x)
}

// CHECK-LABEL: sil @_TF8builtins11destroy_gen
func destroy_gen<T>(x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil @_TF8builtins10assign_pod
func assign_pod(var x: Builtin.Int64, var y: Builtin.RawPointer) {
  // CHECK: alloc_box
  // CHECK: alloc_box
  // CHECK-NOT: alloc_box
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release
  // CHECK: release
  // CHECK-NOT: release
  Builtin.assign(x, y)
  // CHECK: return
}

// CHECK-LABEL: sil @_TF8builtins10assign_obj
func assign_obj(x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil @_TF8builtins12assign_tuple
func assign_tuple(var x: (Builtin.Int64, Builtin.NativeObject),
                  var y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*(Builtin.Int64, Builtin.NativeObject)
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release 
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil @_TF8builtins10assign_gen
func assign_gen<T>(x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil @_TF8builtins8init_pod
func init_pod(x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK-NOT: release [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil @_TF8builtins8init_obj
func init_obj(x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [[ADDR]]
  // CHECK-NOT: release [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil @_TF8builtins8init_gen
func init_gen<T>(x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [initialization]  [[ADDR]]
  Builtin.initialize(x, y)
}

class C {}

// CHECK-LABEL: sil @_TF8builtins22class_to_native_object
func class_to_native_object(c:C) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(c)
}

func class_archetype_to_native_object<T : C>(t: T) -> Builtin.NativeObject {
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil @_TF8builtins34class_existential_to_native_object
func class_existential_to_native_object(t:ClassProto) -> Builtin.NativeObject {
  // CHECK: [[REF:%[0-9]+]] = project_existential_ref [[T:%[0-9]+]] : $ClassProto to $@sil_self ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@sil_self ClassProto to $Builtin.NativeObject
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil @_TF8builtins24class_from_native_object
func class_from_native_object(p: Builtin.NativeObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

func class_archetype_from_native_object<T : C>(p: Builtin.NativeObject) -> T {
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil @_TF8builtins20class_to_raw_pointer
func class_to_raw_pointer(c: C) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

func class_archetype_to_raw_pointer<T : C>(t: T) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(t)
}

// CHECK-LABEL: sil @_TF8builtins18obj_to_raw_pointer
func obj_to_raw_pointer(c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil @_TF8builtins22class_from_raw_pointer
func class_from_raw_pointer(p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil @_TF8builtins20obj_from_raw_pointer
func obj_from_raw_pointer(p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil @_TF8builtins5gep64
func gep64(p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int64(p, i)
}

// CHECK-LABEL: sil @_TF8builtins5gep32
func gep32(p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int32(p, i)
}

// CHECK-LABEL: sil @_TF8builtins8condfail
func condfail(i: Builtin.Int1) {
  Builtin.condfail(i)
  // CHECK: cond_fail {{%.*}} : $Builtin.Int1
}

struct S {}
@objc class O {}
@objc @class_protocol protocol OP1 {}
@objc @class_protocol protocol OP2 {}
protocol P {}

// CHECK-LABEL: sil @_TF8builtins10canBeClass
func canBeClass<T>() {
  // CHECK: integer_literal $Builtin.Int1, -1
  Builtin.canBeClass(O.self)
  // CHECK: integer_literal $Builtin.Int1, -1
  Builtin.canBeClass(OP1.self)
  // -- FIXME: protocol<...> doesn't parse as a value
  typealias ObjCCompo = protocol<OP1, OP2>
  // CHECK: integer_literal $Builtin.Int1, -1
  Builtin.canBeClass(ObjCCompo.self)

  // CHECK: integer_literal $Builtin.Int1, 0
  Builtin.canBeClass(S.self)
  // CHECK: integer_literal $Builtin.Int1, -1
  Builtin.canBeClass(C.self)
  // CHECK: integer_literal $Builtin.Int1, 0
  Builtin.canBeClass(P.self)
  typealias MixedCompo = protocol<OP1, P>
  // CHECK: integer_literal $Builtin.Int1, 0
  Builtin.canBeClass(MixedCompo.self)

  // CHECK: [[CAN_BE:%.*]] = builtin_function_ref "canBeClass"
  // CHECK: apply [[CAN_BE]]<T>
  Builtin.canBeClass(T.self)
}

// FIXME: "T.Type.self" does not parse as an expression

// CHECK-LABEL: sil @_TF8builtins18canBeClassMetatype
func canBeClassMetatype<T>() {
  // CHECK: integer_literal $Builtin.Int1, -1
  typealias OT = O.Type
  Builtin.canBeClass(OT.self)
  // CHECK: integer_literal $Builtin.Int1, -1
  typealias OP1T = OP1.Type
  Builtin.canBeClass(OP1T.self)
  // -- FIXME: protocol<...> doesn't parse as a value
  typealias ObjCCompoT = protocol<OP1, OP2>.Type
  // CHECK: integer_literal $Builtin.Int1, -1
  Builtin.canBeClass(ObjCCompoT.self)

  // CHECK: integer_literal $Builtin.Int1, 0
  typealias ST = S.Type
  Builtin.canBeClass(ST.self)
  // CHECK: integer_literal $Builtin.Int1, -1
  typealias CT = C.Type
  Builtin.canBeClass(CT.self)
  // CHECK: integer_literal $Builtin.Int1, 0
  typealias PT = P.Type
  Builtin.canBeClass(PT.self)
  typealias MixedCompoT = protocol<OP1, P>.Type
  // CHECK: integer_literal $Builtin.Int1, 0
  Builtin.canBeClass(MixedCompoT.self)

  // CHECK: [[CAN_BE:%.*]] = builtin_function_ref "canBeClass"
  // CHECK: apply [[CAN_BE]]<TT>
  typealias TT = T.Type
  Builtin.canBeClass(TT.self)
}

// CHECK-LABEL: sil @_TF8builtins11fixLifetime
func fixLifetime(c: C) {
  // CHECK: fix_lifetime %0 : $C
  Builtin.fixLifetime(c)
}

// CHECK-LABEL: sil @_TF8builtins20assert_configuration
func assert_configuration() -> Builtin.Int32 {
  return Builtin.assert_configuration()
  // CHECK: [[FUNREF:%.*]] = builtin_function_ref "assert_configuration" : $@thin () -> Builtin.Int32
  // CHECK: [[APPLY:%.*]] = apply [[FUNREF]]() : $@thin () -> Builtin.Int32
  // CHECK: return [[APPLY]] : $Builtin.Int32
}

// CHECK-LABEL: sil @_TF8builtins11autorelease
// CHECK: autorelease_value %0
func autorelease(o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil @_TF8builtins11unreachable
// CHECK:         [[UNREACHABLE:%.*]] = builtin_function_ref "unreachable" : $@thin @noreturn () -> () // user: %1
// CHECK:         apply [[UNREACHABLE]]()
// CHECK:         return
// CANONICAL-LABEL: sil @_TF8builtins11unreachableFT_T_ : $@thin @noreturn () -> () {
// CANONICAL-NOT:     builtin_function_ref "unreachable"
// CANONICAL-NOT:     return
// CANONICAL:         unreachable
@noreturn func unreachable() {
  Builtin.unreachable()
}

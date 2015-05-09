// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s -disable-objc-attr-requires-foundation-module | FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Onone -parse-stdlib %s -disable-objc-attr-requires-foundation-module | FileCheck -check-prefix=CANONICAL %s

import Swift

protocol ClassProto : class { }

struct Pointer {
  var value: Builtin.RawPointer
}

// CHECK-LABEL: sil hidden @_TF8builtins3foo
func foo(x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_pod
func load_pod(x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_obj
func load_obj(x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_gen
func load_gen<T>(x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_pod
func move_pod(x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_obj
func move_obj(x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK-NOT: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_gen
func move_gen<T>(x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_pod
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

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_obj
func destroy_obj(x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject, x)
}

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_gen
func destroy_gen<T>(x: Builtin.RawPointer, _: T) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil hidden @_TF8builtins10assign_pod
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

// CHECK-LABEL: sil hidden @_TF8builtins10assign_obj
func assign_obj(x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins12assign_tuple
func assign_tuple(var x: (Builtin.Int64, Builtin.NativeObject),
                  var y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*(Builtin.Int64, Builtin.NativeObject)
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release 
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins10assign_gen
func assign_gen<T>(x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_pod
func init_pod(x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK-NOT: release [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_obj
func init_obj(x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [[ADDR]]
  // CHECK-NOT: release [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_gen
func init_gen<T>(x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*T
  // CHECK: copy_addr [take] {{%.*}} to [initialization]  [[ADDR]]
  Builtin.initialize(x, y)
}

class C {}
class D {}

// CHECK-LABEL: sil hidden @_TF8builtins22class_to_native_object
func class_to_native_object(c:C) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins23class_to_unknown_object
func class_to_unknown_object(c:C) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins32class_archetype_to_native_object
func class_archetype_to_native_object<T : C>(t: T) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins33class_archetype_to_unknown_object
func class_archetype_to_unknown_object<T : C>(t: T) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins34class_existential_to_native_object
func class_existential_to_native_object(t:ClassProto) -> Builtin.NativeObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.NativeObject
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins35class_existential_to_unknown_object
func class_existential_to_unknown_object(t:ClassProto) -> Builtin.UnknownObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.UnknownObject
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins24class_from_native_object
func class_from_native_object(p: Builtin.NativeObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins25class_from_unknown_object
func class_from_unknown_object(p: Builtin.UnknownObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins34class_archetype_from_native_object
func class_archetype_from_native_object<T : C>(p: Builtin.NativeObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $T
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins35class_archetype_from_unknown_object
func class_archetype_from_unknown_object<T : C>(p: Builtin.UnknownObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $T
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins41objc_class_existential_from_native_object
func objc_class_existential_from_native_object(p: Builtin.NativeObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $AnyObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins42objc_class_existential_from_unknown_object
func objc_class_existential_from_unknown_object(p: Builtin.UnknownObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $AnyObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins20class_to_raw_pointer
func class_to_raw_pointer(c: C) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

func class_archetype_to_raw_pointer<T : C>(t: T) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(t)
}

protocol CP: class {}

func existential_to_raw_pointer(p: CP) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins18obj_to_raw_pointer
func obj_to_raw_pointer(c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins22class_from_raw_pointer
func class_from_raw_pointer(p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins20obj_from_raw_pointer
func obj_from_raw_pointer(p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins28unknown_obj_from_raw_pointer
func unknown_obj_from_raw_pointer(p: Builtin.RawPointer) -> Builtin.UnknownObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.UnknownObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins28existential_from_raw_pointer
func existential_from_raw_pointer(p: Builtin.RawPointer) -> AnyObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $AnyObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins5gep64
func gep64(p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int64(p, i)
}

// CHECK-LABEL: sil hidden @_TF8builtins5gep32
func gep32(p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int32(p, i)
}

// CHECK-LABEL: sil hidden @_TF8builtins8condfail
func condfail(i: Builtin.Int1) {
  Builtin.condfail(i)
  // CHECK: cond_fail {{%.*}} : $Builtin.Int1
}

struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: sil hidden @_TF8builtins10canBeClass
func canBeClass<T>(_: T) {
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(O.self)
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(OP1.self)
  // -- FIXME: protocol<...> doesn't parse as a value
  typealias ObjCCompo = protocol<OP1, OP2>
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(ObjCCompo.self)

  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(S.self)
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(C.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(P.self)
  typealias MixedCompo = protocol<OP1, P>
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(MixedCompo.self)

  // CHECK: builtin "canBeClass"<T>
  Builtin.canBeClass(T.self)
}

// FIXME: "T.Type.self" does not parse as an expression

// CHECK-LABEL: sil hidden @_TF8builtins18canBeClassMetatype
func canBeClassMetatype<T>(_: T) {
  // CHECK: integer_literal $Builtin.Int8, 1
  typealias OT = O.Type
  Builtin.canBeClass(OT.self)
  // CHECK: integer_literal $Builtin.Int8, 1
  typealias OP1T = OP1.Type
  Builtin.canBeClass(OP1T.self)
  // -- FIXME: protocol<...> doesn't parse as a value
  typealias ObjCCompoT = protocol<OP1, OP2>.Type
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(ObjCCompoT.self)

  // CHECK: integer_literal $Builtin.Int8, 0
  typealias ST = S.Type
  Builtin.canBeClass(ST.self)
  // CHECK: integer_literal $Builtin.Int8, 1
  typealias CT = C.Type
  Builtin.canBeClass(CT.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  typealias PT = P.Type
  Builtin.canBeClass(PT.self)
  typealias MixedCompoT = protocol<OP1, P>.Type
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(MixedCompoT.self)

  // CHECK: builtin "canBeClass"<T.Type>
  typealias TT = T.Type
  Builtin.canBeClass(TT.self)
}

// CHECK-LABEL: sil hidden @_TF8builtins11fixLifetime
func fixLifetime(c: C) {
  // CHECK: fix_lifetime %0 : $C
  Builtin.fixLifetime(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins20assert_configuration
func assert_configuration() -> Builtin.Int32 {
  return Builtin.assert_configuration()
  // CHECK: [[APPLY:%.*]] = builtin "assert_configuration"() : $Builtin.Int32
  // CHECK: return [[APPLY]] : $Builtin.Int32
}

// CHECK-LABEL: sil hidden @_TF8builtins17assumeNonNegativeFBwBw
func assumeNonNegative(x: Builtin.Word) -> Builtin.Word {
  return Builtin.assumeNonNegative_Word(x)
  // CHECK: [[APPLY:%.*]] = builtin "assumeNonNegative_Word"(%0 : $Builtin.Word) : $Builtin.Word
  // CHECK: return [[APPLY]] : $Builtin.Word
}

// CHECK-LABEL: sil hidden @_TF8builtins11autorelease
// CHECK: autorelease_value %0
func autorelease(o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil hidden @_TF8builtins11unreachable
// CHECK:         builtin "unreachable"()
// CHECK:         return
// CANONICAL-LABEL: sil hidden @_TF8builtins11unreachableFT_T_ : $@convention(thin) @noreturn () -> () {
// CANONICAL-NOT:     builtin "unreachable"
// CANONICAL-NOT:     return
// CANONICAL:         unreachable
@noreturn func unreachable() {
  Builtin.unreachable()
}

// CHECK-LABEL: sil hidden @_TF8builtins15reinterpretCastFCS_1CTBwCS_1DGSqS0___ : $@convention(thin) (@owned C) -> @owned (Builtin.Word, D, Optional<C>)
// CHECK-NEXT:  bb0(%0 : $C):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    strong_retain %0 : $C
// CHECK-NEXT:    unchecked_trivial_bit_cast %0 : $C to $Builtin.Word
// CHECK-NEXT:    unchecked_ref_bit_cast %0 : $C to $D
// CHECK-NEXT:    unchecked_ref_bit_cast %0 : $C to $Optional<C>
// CHECK-NOT:     strong_release
// CHECK-NOT:     release_value
// CHECK:         return
func reinterpretCast(c: C) -> (Builtin.Word, D, C?) {
  return (Builtin.reinterpretCast(c) as Builtin.Word,
          Builtin.reinterpretCast(c) as D,
          Builtin.reinterpretCast(c) as C?)
}

// CHECK-LABEL: sil hidden @_TF8builtins19reinterpretAddrOnlyu0_rFq_q0_
func reinterpretAddrOnly<T, U>(t: T) -> U {
  // CHECK: unchecked_addr_cast {{%.*}} : $*T to $*U
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins28reinterpretAddrOnlyToTrivialurFq_Si
func reinterpretAddrOnlyToTrivial<T>(t: T) -> Int {
  // CHECK: [[ADDR:%.*]] = unchecked_addr_cast [[INPUT:%.*]] : $*T to $*Int
  // CHECK: [[VALUE:%.*]] = load [[ADDR]]
  // CHECK: destroy_addr [[INPUT]]
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins27reinterpretAddrOnlyLoadableurFTSiq__Tq_Si_
func reinterpretAddrOnlyLoadable<T>(a: Int, _ b: T) -> (T, Int) {
  // CHECK: [[BUF:%.*]] = alloc_stack $Int
  // CHECK: store {{%.*}} to [[BUF]]#1
  // CHECK: unchecked_addr_cast [[BUF]]#1 : $*Int to $*T
  return (Builtin.reinterpretCast(a) as T,
  // CHECK: [[RES:%.*]] = unchecked_addr_cast {{%.*}} : $*T to $*Int
  // CHECK: load [[RES]]
          Builtin.reinterpretCast(b) as Int)
}

// CHECK-LABEL: sil hidden @_TF8builtins18castToBridgeObjectFTCS_1CBw_Bb
// CHECK:         [[BO:%.*]] = ref_to_bridge_object {{%.*}} : $C, {{%.*}} : $Builtin.Word
// CHECK:         return [[BO]]
func castToBridgeObject(c: C, _ w: Builtin.Word) -> Builtin.BridgeObject {
  return Builtin.castToBridgeObject(c, w)
}

// CHECK-LABEL: sil hidden @_TF8builtins23castRefFromBridgeObjectFBbCS_1C
// CHECK:         bridge_object_to_ref [[BO:%.*]] : $Builtin.BridgeObject to $C
func castRefFromBridgeObject(bo: Builtin.BridgeObject) -> C {
  return Builtin.castReferenceFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_TF8builtins30castBitPatternFromBridgeObjectFBbBw
// CHECK:         bridge_object_to_word [[BO:%.*]] : $Builtin.BridgeObject to $Builtin.Word
// CHECK:         release [[BO]]
func castBitPatternFromBridgeObject(bo: Builtin.BridgeObject) -> Builtin.Word {
  return Builtin.castBitPatternFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_TF8builtins14markDependenceFTVS_7PointerPS_10ClassProto__S0_ : $@convention(thin) (Pointer, @owned ClassProto) -> Pointer {
// CHECK:         [[T0:%.*]] = mark_dependence %0 : $Pointer on %1 : $ClassProto
// CHECK-NEXT:    strong_release %1 : $ClassProto
// CHECK-NEXT:    return [[T0]] : $Pointer
func markDependence(v: Pointer, _ base: ClassProto) -> Pointer {
  return Builtin.markDependence(v, base)
}

// CHECK-LABEL: sil hidden @_TF8builtins8pinUnpinFBoT_ :
// CHECK-NEXT:  bb0(%0 : $Builtin.NativeObject):
// CHECK-NEXT:    debug_value
func pinUnpin(object : Builtin.NativeObject) {
// CHECK-NEXT:    strong_retain %0 : $Builtin.NativeObject
// CHECK-NEXT:    [[HANDLE:%.*]] = strong_pin %0 : $Builtin.NativeObject
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    strong_release %0 : $Builtin.NativeObject
  let handle : Builtin.NativeObject? = Builtin.tryPin(object)

// CHECK-NEXT:    retain_value [[HANDLE]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:    strong_unpin [[HANDLE]] : $Optional<Builtin.NativeObject>
  Builtin.unpin(handle)

// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    release_value [[HANDLE]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:    strong_release %0 : $Builtin.NativeObject
// CHECK-NEXT:    [[T0:%.*]] = tuple ()
// CHECK-NEXT:    return [[T0]] : $()
}

// CHECK-LABEL: sil hidden @_TF8builtins19allocateValueBufferFRBBBp : $@convention(thin) (@inout Builtin.UnsafeValueBuffer) -> Builtin.RawPointer
// CHECK-NEXT:  bb0([[BUFFER:%.*]] : $*Builtin.UnsafeValueBuffer):
// CHECK-NEXT:    metatype $@thin Int.Type
// CHECK-NEXT:    [[T0:%.*]] = alloc_value_buffer $Int in [[BUFFER]] : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:    [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK-NEXT:    return [[T1]] : $Builtin.RawPointer
func allocateValueBuffer(inout buffer: Builtin.UnsafeValueBuffer) -> Builtin.RawPointer {
  return Builtin.allocValueBuffer(&buffer, Int.self)
}

// CHECK-LABEL: sil hidden @_TF8builtins18projectValueBufferFRBBBp : $@convention(thin) (@inout Builtin.UnsafeValueBuffer) -> Builtin.RawPointer
// CHECK-NEXT:  bb0([[BUFFER:%.*]] : $*Builtin.UnsafeValueBuffer):
// CHECK-NEXT:    metatype $@thin Int.Type
// CHECK-NEXT:    [[T0:%.*]] = project_value_buffer $Int in [[BUFFER]] : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:    [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK-NEXT:    return [[T1]] : $Builtin.RawPointer
func projectValueBuffer(inout buffer: Builtin.UnsafeValueBuffer) -> Builtin.RawPointer {
  return Builtin.projectValueBuffer(&buffer, Int.self)
}

// CHECK-LABEL: sil hidden @_TF8builtins18deallocValueBufferFRBBT_ : $@convention(thin) (@inout Builtin.UnsafeValueBuffer) -> ()
// CHECK-NEXT:  bb0([[BUFFER:%.*]] : $*Builtin.UnsafeValueBuffer):
// CHECK-NEXT:    metatype $@thin Int.Type
// CHECK-NEXT:    dealloc_value_buffer $Int in [[BUFFER]] : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    [[T0:%.*]] = tuple ()
// CHECK-NEXT:    return [[T0]] : $()
func deallocValueBuffer(inout buffer: Builtin.UnsafeValueBuffer) -> () {
  Builtin.deallocValueBuffer(&buffer, Int.self)
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// NativeObject
// CHECK-LABEL: sil hidden @_TF8builtins8isUniqueFRGSqBo_Sb : $@convention(thin) (@inout Optional<Builtin.NativeObject>) -> Bool
// CHECK-NEXT: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.NativeObject>
// CHECK-NEXT: copy_addr %0 to [initialization] [[BOX]]#1 : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[BOX]]#1 : $*Optional<Builtin.NativeObject>
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Optional<Builtin.NativeObject>
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(inout ref: Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUniqueFRBoSb : $@convention(thin) (@inout Builtin.NativeObject) -> Bool
// CHECK-NEXT: bb0(%0 : $*Builtin.NativeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.NativeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[BOX]]#1 : $*Builtin.NativeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.NativeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(inout ref: Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject pinned
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinnedFRGSqBo_Sb : $@convention(thin) (@inout Optional<Builtin.NativeObject>) -> Bool
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.NativeObject>
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[BOX]]#1 : $*Optional<Builtin.NativeObject>
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Optional<Builtin.NativeObject>
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned(inout ref: Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// NativeObject pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinnedFRBoSb : $@convention(thin) (@inout Builtin.NativeObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.NativeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[BOX]]#1 : $*Builtin.NativeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.NativeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned(inout ref: Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// UnknownObject (ObjC)
// CHECK-LABEL: sil hidden @_TF8builtins8isUniqueFRGSqBO_Sb : $@convention(thin) (@inout Optional<Builtin.UnknownObject>) -> Bool
// CHECK: bb0(%0 : $*Optional<Builtin.UnknownObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.UnknownObject>
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Optional<Builtin.UnknownObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[BOX]]#1 : $*Optional<Builtin.UnknownObject>
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Optional<Builtin.UnknownObject>
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(inout ref: Builtin.UnknownObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUniqueFRBOSb : $@convention(thin) (@inout Builtin.UnknownObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.UnknownObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.UnknownObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[BOX]]#1 : $*Builtin.UnknownObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.UnknownObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(inout ref: Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinnedFRBOSb : $@convention(thin) (@inout Builtin.UnknownObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.UnknownObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.UnknownObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[BOX]]#1 : $*Builtin.UnknownObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.UnknownObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned(inout ref: Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUniqueFRBbSb : $@convention(thin) (@inout Builtin.BridgeObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(inout ref: Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// BridgeObject pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinnedFRBbSb : $@convention(thin) (@inout Builtin.BridgeObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned(inout ref: Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull native
// CHECK-LABEL: sil hidden @_TF8builtins15isUnique_nativeFRBbSb : $@convention(thin) (@inout Builtin.BridgeObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[BOX]]#1 : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[CAST]] : $*Builtin.NativeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUnique_native(inout ref: Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique_native(&ref))
}

// BridgeObject pinned nonNull native
// CHECK-LABEL: sil hidden @_TF8builtins23isUniqueOrPinned_nativeFRBbSb : $@convention(thin) (@inout Builtin.BridgeObject) -> Bool
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK: copy_addr %0 to [initialization] [[BOX]]#1 : $*Builtin.BridgeObject
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[BOX]]#1 : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[CAST]] : $*Builtin.NativeObject
// CHECK: copy_addr [[BOX]]#1 to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned_native(inout ref: Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned_native(&ref))
}

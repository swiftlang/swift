// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Onone -parse-stdlib %s -disable-objc-attr-requires-foundation-module | %FileCheck -check-prefix=CANONICAL %s

import Swift

protocol ClassProto : class { }

struct Pointer {
  var value: Builtin.RawPointer
}

// CHECK-LABEL: sil hidden @_TF8builtins3foo
func foo(_ x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_pod
func load_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_obj
func load_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins12load_raw_pod
func load_raw_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins12load_raw_obj
func load_raw_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8load_gen
func load_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_pod
func move_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_obj
func move_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [[ADDR]]
  // CHECK-NOT: retain [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins8move_gen
func move_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_pod
func destroy_pod(_ x: Builtin.RawPointer) {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box
  // CHECK-NOT: pointer_to_address
  // CHECK-NOT: destroy_addr
  // CHECK-NOT: release
  // CHECK: release [[XBOX]] : $@box
  // CHECK-NOT: release
  return Builtin.destroy(Builtin.Int64, x)
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_obj
func destroy_obj(_ x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject, x)
}

// CHECK-LABEL: sil hidden @_TF8builtins11destroy_gen
func destroy_gen<T>(_ x: Builtin.RawPointer, _: T) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil hidden @_TF8builtins10assign_pod
func assign_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  var x = x
  var y = y
  // CHECK: alloc_box
  // CHECK: alloc_box
  // CHECK-NOT: alloc_box
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release
  // CHECK: release
  // CHECK-NOT: release
  Builtin.assign(x, y)
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF8builtins10assign_obj
func assign_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: release
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins12assign_tuple
func assign_tuple(_ x: (Builtin.Int64, Builtin.NativeObject),
                  y: Builtin.RawPointer) {
  var x = x
  var y = y
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*(Builtin.Int64, Builtin.NativeObject)
  // CHECK: [[T0:%.*]] = tuple_element_addr [[ADDR]]
  // CHECK: assign {{%.*}} to [[T0]]
  // CHECK: [[T0:%.*]] = tuple_element_addr [[ADDR]]
  // CHECK: assign {{%.*}} to [[T0]]
  // CHECK: release 
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins10assign_gen
func assign_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_pod
func init_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [[ADDR]]
  // CHECK-NOT: release [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_obj
func init_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [[ADDR]]
  // CHECK-NOT: release [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_TF8builtins8init_gen
func init_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] {{%.*}} to [initialization]  [[ADDR]]
  Builtin.initialize(x, y)
}

class C {}
class D {}

// CHECK-LABEL: sil hidden @_TF8builtins22class_to_native_object
func class_to_native_object(_ c:C) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins23class_to_unknown_object
func class_to_unknown_object(_ c:C) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins32class_archetype_to_native_object
func class_archetype_to_native_object<T : C>(_ t: T) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins33class_archetype_to_unknown_object
func class_archetype_to_unknown_object<T : C>(_ t: T) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins34class_existential_to_native_object
func class_existential_to_native_object(_ t:ClassProto) -> Builtin.NativeObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.NativeObject
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins35class_existential_to_unknown_object
func class_existential_to_unknown_object(_ t:ClassProto) -> Builtin.UnknownObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.UnknownObject
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins24class_from_native_object
func class_from_native_object(_ p: Builtin.NativeObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins25class_from_unknown_object
func class_from_unknown_object(_ p: Builtin.UnknownObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins34class_archetype_from_native_object
func class_archetype_from_native_object<T : C>(_ p: Builtin.NativeObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $T
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins35class_archetype_from_unknown_object
func class_archetype_from_unknown_object<T : C>(_ p: Builtin.UnknownObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $T
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins41objc_class_existential_from_native_object
func objc_class_existential_from_native_object(_ p: Builtin.NativeObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $AnyObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins42objc_class_existential_from_unknown_object
func objc_class_existential_from_unknown_object(_ p: Builtin.UnknownObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $AnyObject
  // CHECK-NOT: release [[C]]
  // CHECK-NOT: release [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins20class_to_raw_pointer
func class_to_raw_pointer(_ c: C) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

func class_archetype_to_raw_pointer<T : C>(_ t: T) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(t)
}

protocol CP: class {}

func existential_to_raw_pointer(_ p: CP) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins18obj_to_raw_pointer
func obj_to_raw_pointer(_ c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil hidden @_TF8builtins22class_from_raw_pointer
func class_from_raw_pointer(_ p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(_ p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins20obj_from_raw_pointer
func obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins28unknown_obj_from_raw_pointer
func unknown_obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.UnknownObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.UnknownObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins28existential_from_raw_pointer
func existential_from_raw_pointer(_ p: Builtin.RawPointer) -> AnyObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $AnyObject
  // CHECK: retain [[C]]
  // CHECK: return [[C]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_TF8builtins5gep64
func gep64(_ p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int64(p, i)
}

// CHECK-LABEL: sil hidden @_TF8builtins5gep32
func gep32(_ p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gep_Int32(p, i)
}

// CHECK-LABEL: sil hidden @_TF8builtins8condfail
func condfail(_ i: Builtin.Int1) {
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
  // -- FIXME: 'OP1 & OP2' doesn't parse as a value
  typealias ObjCCompo = OP1 & OP2
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(ObjCCompo.self)

  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(S.self)
  // CHECK: integer_literal $Builtin.Int8, 1
  Builtin.canBeClass(C.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(P.self)
  typealias MixedCompo = OP1 & P
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(MixedCompo.self)

  // CHECK: builtin "canBeClass"<T>
  Builtin.canBeClass(T.self)
}

// FIXME: "T.Type.self" does not parse as an expression

// CHECK-LABEL: sil hidden @_TF8builtins18canBeClassMetatype
func canBeClassMetatype<T>(_: T) {
  // CHECK: integer_literal $Builtin.Int8, 0
  typealias OT = O.Type
  Builtin.canBeClass(OT.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  typealias OP1T = OP1.Type
  Builtin.canBeClass(OP1T.self)
  // -- FIXME: 'OP1 & OP2' doesn't parse as a value
  typealias ObjCCompoT = (OP1 & OP2).Type
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(ObjCCompoT.self)

  // CHECK: integer_literal $Builtin.Int8, 0
  typealias ST = S.Type
  Builtin.canBeClass(ST.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  typealias CT = C.Type
  Builtin.canBeClass(CT.self)
  // CHECK: integer_literal $Builtin.Int8, 0
  typealias PT = P.Type
  Builtin.canBeClass(PT.self)
  typealias MixedCompoT = (OP1 & P).Type
  // CHECK: integer_literal $Builtin.Int8, 0
  Builtin.canBeClass(MixedCompoT.self)

  // CHECK: integer_literal $Builtin.Int8, 0
  typealias TT = T.Type
  Builtin.canBeClass(TT.self)
}

// CHECK-LABEL: sil hidden @_TF8builtins11fixLifetime
func fixLifetime(_ c: C) {
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
func assumeNonNegative(_ x: Builtin.Word) -> Builtin.Word {
  return Builtin.assumeNonNegative_Word(x)
  // CHECK: [[APPLY:%.*]] = builtin "assumeNonNegative_Word"(%0 : $Builtin.Word) : $Builtin.Word
  // CHECK: return [[APPLY]] : $Builtin.Word
}

// CHECK-LABEL: sil hidden @_TF8builtins11autorelease
// CHECK: autorelease_value %0
func autorelease(_ o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil hidden @_TF8builtins11unreachable
// CHECK:         builtin "unreachable"()
// CHECK:         return
// CANONICAL-LABEL: sil hidden @_TF8builtins11unreachableFT_T_ : $@convention(thin) () -> () {
func unreachable() {
  Builtin.unreachable()
}

// CHECK-LABEL: sil hidden @_TF8builtins15reinterpretCastFTCS_1C1xBw_TBwCS_1DGSqS0__S0__ : $@convention(thin) (@owned C, Builtin.Word) -> (Builtin.Word, @owned D, @owned Optional<C>, @owned C)
// CHECK:       bb0(%0 : $C, %1 : $Builtin.Word):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    strong_retain %0 : $C
// CHECK-NEXT:    unchecked_trivial_bit_cast %0 : $C to $Builtin.Word
// CHECK-NEXT:    unchecked_ref_cast %0 : $C to $D
// CHECK-NEXT:    unchecked_ref_cast %0 : $C to $Optional<C>
// CHECK-NEXT:    unchecked_bitwise_cast %1 : $Builtin.Word to $C
// CHECK-NEXT:    strong_retain %{{.*}} : $C
// CHECK-NOT:     strong_retain
// CHECK-NOT:     strong_release
// CHECK-NOT:     release_value
// CHECK:         return
func reinterpretCast(_ c: C, x: Builtin.Word) -> (Builtin.Word, D, C?, C) {
  return (Builtin.reinterpretCast(c) as Builtin.Word,
          Builtin.reinterpretCast(c) as D,
          Builtin.reinterpretCast(c) as C?,
          Builtin.reinterpretCast(x) as C)
}

// CHECK-LABEL: sil hidden @_TF8builtins19reinterpretAddrOnly
func reinterpretAddrOnly<T, U>(_ t: T) -> U {
  // CHECK: unchecked_addr_cast {{%.*}} : $*T to $*U
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins28reinterpretAddrOnlyToTrivial
func reinterpretAddrOnlyToTrivial<T>(_ t: T) -> Int {
  // CHECK: [[ADDR:%.*]] = unchecked_addr_cast [[INPUT:%.*]] : $*T to $*Int
  // CHECK: [[VALUE:%.*]] = load [[ADDR]]
  // CHECK: destroy_addr [[INPUT]]
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins27reinterpretAddrOnlyLoadable
func reinterpretAddrOnlyLoadable<T>(_ a: Int, _ b: T) -> (T, Int) {
  // CHECK: [[BUF:%.*]] = alloc_stack $Int
  // CHECK: store {{%.*}} to [[BUF]]
  // CHECK: [[RES1:%.*]] = unchecked_addr_cast [[BUF]] : $*Int to $*T
  // CHECK: copy_addr [[RES1]] to [initialization]
  return (Builtin.reinterpretCast(a) as T,
  // CHECK: [[RES:%.*]] = unchecked_addr_cast {{%.*}} : $*T to $*Int
  // CHECK: load [[RES]]
          Builtin.reinterpretCast(b) as Int)
}

// CHECK-LABEL: sil hidden @_TF8builtins18castToBridgeObject
// CHECK:         [[BO:%.*]] = ref_to_bridge_object {{%.*}} : $C, {{%.*}} : $Builtin.Word
// CHECK:         return [[BO]]
func castToBridgeObject(_ c: C, _ w: Builtin.Word) -> Builtin.BridgeObject {
  return Builtin.castToBridgeObject(c, w)
}

// CHECK-LABEL: sil hidden @_TF8builtins23castRefFromBridgeObject
// CHECK:         bridge_object_to_ref [[BO:%.*]] : $Builtin.BridgeObject to $C
func castRefFromBridgeObject(_ bo: Builtin.BridgeObject) -> C {
  return Builtin.castReferenceFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_TF8builtins30castBitPatternFromBridgeObject
// CHECK:         bridge_object_to_word [[BO:%.*]] : $Builtin.BridgeObject to $Builtin.Word
// CHECK:         release [[BO]]
func castBitPatternFromBridgeObject(_ bo: Builtin.BridgeObject) -> Builtin.Word {
  return Builtin.castBitPatternFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_TF8builtins8pinUnpin
// CHECK:       bb0(%0 : $Builtin.NativeObject):
// CHECK-NEXT:    debug_value
func pinUnpin(_ object : Builtin.NativeObject) {
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

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// NativeObject
// CHECK-LABEL: sil hidden @_TF8builtins8isUnique
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.NativeObject>
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT: copy_addr %0 to [initialization] [[PB]] : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[PB]] : $*Optional<Builtin.NativeObject>
// CHECK: copy_addr [[PB]] to %0 : $*Optional<Builtin.NativeObject>
// CHECK-NEXT: strong_release [[BOX]] : $@box Optional<Builtin.NativeObject>
// CHECK-NEXT: return
func isUnique(_ ref: inout Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUnique
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.NativeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[PB]] : $*Builtin.NativeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.NativeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.NativeObject
// CHECK-NEXT: return
func isUnique(_ ref: inout Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject pinned
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinned
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.NativeObject>
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[PB]] : $*Optional<Builtin.NativeObject>
// CHECK: copy_addr [[PB]] to %0 : $*Optional<Builtin.NativeObject>
// CHECK-NEXT: strong_release [[BOX]] : $@box Optional<Builtin.NativeObject>
// CHECK-NEXT: return
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// NativeObject pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinned
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.NativeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[PB]] : $*Builtin.NativeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.NativeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.NativeObject
// CHECK-NEXT: return
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// UnknownObject (ObjC)
// CHECK-LABEL: sil hidden @_TF8builtins8isUnique
// CHECK: bb0(%0 : $*Optional<Builtin.UnknownObject>):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Optional<Builtin.UnknownObject>
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Optional<Builtin.UnknownObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[PB]] : $*Optional<Builtin.UnknownObject>
// CHECK: copy_addr [[PB]] to %0 : $*Optional<Builtin.UnknownObject>
// CHECK-NEXT: strong_release [[BOX]] : $@box Optional<Builtin.UnknownObject>
// CHECK-NEXT: return
func isUnique(_ ref: inout Builtin.UnknownObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUnique
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.UnknownObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.UnknownObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[PB]] : $*Builtin.UnknownObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.UnknownObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.UnknownObject
// CHECK-NEXT: return
func isUnique(_ ref: inout Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinned
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.UnknownObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.UnknownObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[PB]] : $*Builtin.UnknownObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.UnknownObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.UnknownObject
// CHECK-NEXT: return
func isUniqueOrPinned(_ ref: inout Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull
// CHECK-LABEL: sil hidden @_TF8builtins8isUnique
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.BridgeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[PB]] : $*Builtin.BridgeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.BridgeObject
// CHECK-NEXT: return
func isUnique(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// BridgeObject pinned nonNull
// CHECK-LABEL: sil hidden @_TF8builtins16isUniqueOrPinned
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.BridgeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[PB]] : $*Builtin.BridgeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.BridgeObject
// CHECK-NEXT: return
func isUniqueOrPinned(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull native
// CHECK-LABEL: sil hidden @_TF8builtins15isUnique_native
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.BridgeObject
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[PB]] : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique [[CAST]] : $*Builtin.NativeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.BridgeObject
// CHECK-NEXT: return
func isUnique_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique_native(&ref))
}

// BridgeObject pinned nonNull native
// CHECK-LABEL: sil hidden @_TF8builtins23isUniqueOrPinned_native
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box $Builtin.BridgeObject
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK: copy_addr %0 to [initialization] [[PB]] : $*Builtin.BridgeObject
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[PB]] : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[CAST]] : $*Builtin.NativeObject
// CHECK: copy_addr [[PB]] to %0 : $*Builtin.BridgeObject
// CHECK-NEXT: strong_release [[BOX]] : $@box Builtin.BridgeObject
// CHECK-NEXT: return
func isUniqueOrPinned_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned_native(&ref))
}

// ----------------------------------------------------------------------------
// Builtin.castReference
// ----------------------------------------------------------------------------

class A {}
protocol PUnknown {}
protocol PClass : class {}

// CHECK-LABEL: sil hidden @_TF8builtins19refcast_generic_any
// CHECK: unchecked_ref_cast_addr  T in %{{.*}} : $*T to AnyObject in %{{.*}} : $*AnyObject
func refcast_generic_any<T>(_ o: T) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_TF8builtins17refcast_class_any
// CHECK: unchecked_ref_cast %0 : $A to $AnyObject
// CHECK-NEXT: return
func refcast_class_any(_ o: A) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_TF8builtins20refcast_punknown_any
// CHECK: unchecked_ref_cast_addr PUnknown in %{{.*}} : $*PUnknown to AnyObject in %{{.*}} : $*AnyObject
func refcast_punknown_any(_ o: PUnknown) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_TF8builtins18refcast_pclass_any
// CHECK: unchecked_ref_cast %0 : $PClass to $AnyObject
// CHECK-NEXT: return
func refcast_pclass_any(_ o: PClass) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_TF8builtins20refcast_any_punknown
// CHECK: unchecked_ref_cast_addr AnyObject in %{{.*}} : $*AnyObject to PUnknown in %{{.*}} : $*PUnknown
func refcast_any_punknown(_ o: AnyObject) -> PUnknown {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_TF8builtins22unsafeGuaranteed_class
// CHECK: bb0([[P:%.*]] : $A):
// CHECK:   strong_retain  [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<A>([[P]] : $A)
// CHECK:   [[R:%.*]] = tuple_extract [[T]] : $(A, Builtin.Int8), 0
// CHECK:   [[K:%.*]] = tuple_extract [[T]] : $(A, Builtin.Int8), 1
// CHECK:   strong_release [[R]] : $A
// CHECK:   return [[P]] : $A
// CHECK: }
func unsafeGuaranteed_class(_ a: A) -> A {
  Builtin.unsafeGuaranteed(a)
  return a
}

// CHECK-LABEL: _TF8builtins24unsafeGuaranteed_generic
// CHECK: bb0([[P:%.*]] : $T):
// CHECK:   strong_retain  [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P]] : $T)
// CHECK:   [[R:%.*]] = tuple_extract [[T]] : $(T, Builtin.Int8), 0
// CHECK:   [[K:%.*]] = tuple_extract [[T]] : $(T, Builtin.Int8), 1
// CHECK:   strong_release [[R]] : $T
// CHECK:   return [[P]] : $T
// CHECK: }
func unsafeGuaranteed_generic<T: AnyObject> (_ a: T) -> T {
  Builtin.unsafeGuaranteed(a)
  return a
}

// CHECK_LABEL: sil hidden @_TF8builtins31unsafeGuaranteed_generic_return
// CHECK: bb0([[P:%.*]] : $T):
// CHECK:   strong_retain [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P]] : $T)
// CHECK:   [[R]] = tuple_extract [[T]] : $(T, Builtin.Int8), 0
// CHECK:   [[K]] = tuple_extract [[T]] : $(T, Builtin.Int8), 1
// CHECK:   strong_release [[P]]
// CHECK:   [[S:%.*]] = tuple ([[R]] : $T, [[K]] : $Builtin.Int8)
// CHECK:   return [[S]] : $(T, Builtin.Int8)
// CHECK: }
func unsafeGuaranteed_generic_return<T: AnyObject> (_ a: T) -> (T, Builtin.Int8) {
  return Builtin.unsafeGuaranteed(a)
}

// CHECK-LABEL: sil hidden @_TF8builtins19unsafeGuaranteedEnd
// CHECK: bb0([[P:%.*]] : $Builtin.Int8):
// CHECK:   builtin "unsafeGuaranteedEnd"([[P]] : $Builtin.Int8)
// CHECK:   [[S:%.*]] = tuple ()
// CHECK:   return [[S]] : $()
// CHECK: }
func unsafeGuaranteedEnd(_ t: Builtin.Int8) {
  Builtin.unsafeGuaranteedEnd(t)
}

// CHECK-LABEL: sil hidden @_TF8builtins10bindMemory
// CHECK: bb0([[P:%.*]] : $Builtin.RawPointer, [[I:%.*]] : $Builtin.Word, [[T:%.*]] : $@thick T.Type):
// CHECK: bind_memory [[P]] : $Builtin.RawPointer, [[I]] : $Builtin.Word to $*T
// CHECK:   return {{%.*}} : $()
// CHECK: }
func bindMemory<T>(ptr: Builtin.RawPointer, idx: Builtin.Word, _: T.Type) {
  Builtin.bindMemory(ptr, idx, T.self)
}

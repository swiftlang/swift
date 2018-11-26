// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-emit-sil -enable-sil-ownership -Onone -parse-stdlib %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=CANONICAL %s

import Swift

protocol ClassProto : class { }

struct Pointer {
  var value: Builtin.RawPointer
}

// CHECK-LABEL: sil hidden @$s8builtins3foo{{[_0-9a-zA-Z]*}}F
func foo(_ x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins8load_pod{{[_0-9a-zA-Z]*}}F
func load_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @$s8builtins8load_obj{{[_0-9a-zA-Z]*}}F
func load_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @$s8builtins12load_raw_pod{{[_0-9a-zA-Z]*}}F
func load_raw_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @$s8builtins12load_raw_obj{{[_0-9a-zA-Z]*}}F
func load_raw_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @$s8builtins18load_invariant_pod{{[_0-9a-zA-Z]*}}F
func load_invariant_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [invariant] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadInvariant(x)
}

// CHECK-LABEL: sil hidden @$s8builtins18load_invariant_obj{{[_0-9a-zA-Z]*}}F
func load_invariant_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [invariant] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadInvariant(x)
}

// CHECK-LABEL: sil hidden @$s8builtins8load_gen{{[_0-9a-zA-Z]*}}F
func load_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @$s8builtins8move_pod{{[_0-9a-zA-Z]*}}F
func move_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @$s8builtins8move_obj{{[_0-9a-zA-Z]*}}F
func move_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [take] [[ADDR]]
  // CHECK-NOT: copy_value [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @$s8builtins8move_gen{{[_0-9a-zA-Z]*}}F
func move_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @$s8builtins11destroy_pod{{[_0-9a-zA-Z]*}}F
func destroy_pod(_ x: Builtin.RawPointer) {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box
  // CHECK-NOT: pointer_to_address
  // CHECK-NOT: destroy_addr
  // CHECK-NOT: destroy_value
  // CHECK: destroy_value [[XBOX]] : ${{.*}}{
  // CHECK-NOT: destroy_value
  return Builtin.destroy(Builtin.Int64.self, x)
  // CHECK: return
}

// CHECK-LABEL: sil hidden @$s8builtins11destroy_obj{{[_0-9a-zA-Z]*}}F
func destroy_obj(_ x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject.self, x)
}

// CHECK-LABEL: sil hidden @$s8builtins11destroy_gen{{[_0-9a-zA-Z]*}}F
func destroy_gen<T>(_ x: Builtin.RawPointer, _: T) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil hidden @$s8builtins10assign_pod{{[_0-9a-zA-Z]*}}F
func assign_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  var x = x
  var y = y
  // CHECK: alloc_box
  // CHECK: alloc_box
  // CHECK-NOT: alloc_box
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: destroy_value
  // CHECK: destroy_value
  // CHECK-NOT: destroy_value
  Builtin.assign(x, y)
  // CHECK: return
}

// CHECK-LABEL: sil hidden @$s8builtins10assign_obj{{[_0-9a-zA-Z]*}}F
func assign_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK-NOT: destroy_value
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins12assign_tuple{{[_0-9a-zA-Z]*}}F
func assign_tuple(_ x: (Builtin.Int64, Builtin.NativeObject),
                  y: Builtin.RawPointer) {
  var x = x
  var y = y
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*(Builtin.Int64, Builtin.NativeObject)
  // CHECK: [[T0:%.*]] = tuple_element_addr [[ADDR]]
  // CHECK: assign {{%.*}} to [[T0]]
  // CHECK: [[T0:%.*]] = tuple_element_addr [[ADDR]]
  // CHECK: assign {{%.*}} to [[T0]]
  // CHECK: destroy_value
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins10assign_gen{{[_0-9a-zA-Z]*}}F
func assign_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins8init_pod{{[_0-9a-zA-Z]*}}F
func init_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [trivial] [[ADDR]]
  // CHECK-NOT: destroy_value [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins8init_obj{{[_0-9a-zA-Z]*}}F
func init_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [init] [[ADDR]]
  // CHECK-NOT: destroy_value [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @$s8builtins8init_gen{{[_0-9a-zA-Z]*}}F
func init_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[OTHER_LOC:%.*]] to [initialization]  [[ADDR]]
  // CHECK-NOT: destroy_addr [[OTHER_LOC]]
  Builtin.initialize(x, y)
}

class C {}
class D {}

// CHECK-LABEL: sil hidden @$s8builtins22class_to_native_object{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $C):
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT:   [[OBJ:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $C to $Builtin.NativeObject
// CHECK-NEXT:   return [[OBJ]]
func class_to_native_object(_ c:C) -> Builtin.NativeObject {
  return Builtin.castToNativeObject(c)
}

// CHECK-LABEL: sil hidden @$s8builtins32class_archetype_to_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_to_native_object<T : C>(_ t: T) -> Builtin.NativeObject {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[COPY]] : $T to $Builtin.NativeObject
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @$s8builtins34class_existential_to_native_object{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $ClassProto):
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT:   [[REF:%[0-9]+]] = open_existential_ref [[ARG_COPY]] : $ClassProto
// CHECK-NEXT:   [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.NativeObject
// CHECK-NEXT:   return [[PTR]]
func class_existential_to_native_object(_ t:ClassProto) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(t)
}

// CHECK-LABEL: sil hidden @$s8builtins24class_from_native_object{{[_0-9a-zA-Z]*}}F
func class_from_native_object(_ p: Builtin.NativeObject) -> C {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $C
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @$s8builtins34class_archetype_from_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_from_native_object<T : C>(_ p: Builtin.NativeObject) -> T {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $T
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @$s8builtins41objc_class_existential_from_native_object{{[_0-9a-zA-Z]*}}F
func objc_class_existential_from_native_object(_ p: Builtin.NativeObject) -> AnyObject {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $AnyObject
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @$s8builtins20class_to_raw_pointer{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @$s8builtins18obj_to_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_to_raw_pointer(_ c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil hidden @$s8builtins22class_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func class_from_raw_pointer(_ p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(_ p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @$s8builtins20obj_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @$s8builtins28existential_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func existential_from_raw_pointer(_ p: Builtin.RawPointer) -> AnyObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $AnyObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @$s8builtins9gep_raw64{{[_0-9a-zA-Z]*}}F
func gep_raw64(_ p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int64(p, i)
}

// CHECK-LABEL: sil hidden @$s8builtins9gep_raw32{{[_0-9a-zA-Z]*}}F
func gep_raw32(_ p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int32(p, i)
}

// CHECK-LABEL: sil hidden @$s8builtins3gep{{[_0-9a-zA-Z]*}}F
func gep<Elem>(_ p: Builtin.RawPointer, i: Builtin.Word, e: Elem.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[GEP:%.*]] = index_addr [[P2A]] : $*Elem, %1 : $Builtin.Word
  // CHECK: [[A2P:%.*]] = address_to_pointer [[GEP]]
  // CHECK: return [[A2P]]
  return Builtin.gep_Word(p, i, e)
}

public final class Header { }

// CHECK-LABEL: sil hidden @$s8builtins20allocWithTailElems_1{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_1<T>(n: Builtin.Word, ty: T.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T * %0 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_1(Header.self, n, ty)
}

// CHECK-LABEL: sil hidden @$s8builtins20allocWithTailElems_3{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_3<T1, T2, T3>(n1: Builtin.Word, ty1: T1.Type, n2: Builtin.Word, ty2: T2.Type, n3: Builtin.Word, ty3: T3.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T1 * %0 : $Builtin.Word] [tail_elems $T2 * %2 : $Builtin.Word] [tail_elems $T3 * %4 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_3(Header.self, n1, ty1, n2, ty2, n3, ty3)
}

// CHECK-LABEL: sil hidden @$s8builtins16projectTailElems{{[_0-9a-zA-Z]*}}F
func projectTailElems<T>(h: Header, ty: T.Type) -> Builtin.RawPointer {
  // CHECK: bb0([[ARG1:%.*]] : @guaranteed $Header
  // CHECK:   [[TA:%.*]] = ref_tail_addr [[ARG1]] : $Header
  // CHECK:   [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK:   return [[A2P]]
  return Builtin.projectTailElems(h, ty)
}
// CHECK: } // end sil function '$s8builtins16projectTailElems1h2tyBpAA6HeaderC_xmtlF'

// CHECK-LABEL: sil hidden @$s8builtins11getTailAddr{{[_0-9a-zA-Z]*}}F
func getTailAddr<T1, T2>(start: Builtin.RawPointer, i: Builtin.Word, ty1: T1.Type, ty2: T2.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[TA:%.*]] = tail_addr [[P2A]] : $*T1, %1 : $Builtin.Word, $T2
  // CHECK: [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK: return [[A2P]]
  return Builtin.getTailAddr_Word(start, i, ty1, ty2)
}

// CHECK-LABEL: sil hidden @$s8builtins25beginUnpairedModifyAccess{{[_0-9a-zA-Z]*}}F
func beginUnpairedModifyAccess<T1>(address: Builtin.RawPointer, scratch: Builtin.RawPointer, ty1: T1.Type) {
  // CHECK: [[P2A_ADDR:%.*]] = pointer_to_address %0
  // CHECK: [[P2A_SCRATCH:%.*]] = pointer_to_address %1
  // CHECK: begin_unpaired_access [modify] [dynamic] [builtin] [[P2A_ADDR]] : $*T1, [[P2A_SCRATCH]] : $*Builtin.UnsafeValueBuffer
  // CHECK: [[RESULT:%.*]] = tuple ()
  // CHECK: [[RETURN:%.*]] = tuple ()
  // CHECK: return [[RETURN]] : $()

  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK-LABEL: sil hidden @$s8builtins30performInstantaneousReadAccess{{[_0-9a-zA-Z]*}}F
func performInstantaneousReadAccess<T1>(address: Builtin.RawPointer, scratch: Builtin.RawPointer, ty1: T1.Type) {
  // CHECK: [[P2A_ADDR:%.*]] = pointer_to_address %0
  // CHECK: [[SCRATCH:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: begin_unpaired_access [read] [dynamic] [no_nested_conflict] [builtin] [[P2A_ADDR]] : $*T1, [[SCRATCH]] : $*Builtin.UnsafeValueBuffer
  // CHECK-NOT: end_{{.*}}access
  // CHECK: [[RESULT:%.*]] = tuple ()
  // CHECK: [[RETURN:%.*]] = tuple ()
  // CHECK: return [[RETURN]] : $()

  Builtin.performInstantaneousReadAccess(address, ty1);
}

// CHECK-LABEL: sil hidden @$s8builtins8condfail{{[_0-9a-zA-Z]*}}F
func condfail(_ i: Builtin.Int1) {
  Builtin.condfail(i)
  // CHECK: cond_fail {{%.*}} : $Builtin.Int1
}

struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: sil hidden @$s8builtins10canBeClass{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @$s8builtins18canBeClassMetatype{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @$s8builtins11fixLifetimeyyAA1CCF : $@convention(thin) (@guaranteed C) -> () {
func fixLifetime(_ c: C) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $C):
  // CHECK:   fix_lifetime [[ARG]] : $C
  Builtin.fixLifetime(c)
}
// CHECK: } // end sil function '$s8builtins11fixLifetimeyyAA1CCF'

// CHECK-LABEL: sil hidden @$s8builtins20assert_configuration{{[_0-9a-zA-Z]*}}F
func assert_configuration() -> Builtin.Int32 {
  return Builtin.assert_configuration()
  // CHECK: [[APPLY:%.*]] = builtin "assert_configuration"() : $Builtin.Int32
  // CHECK: return [[APPLY]] : $Builtin.Int32
}

// CHECK-LABEL: sil hidden @$s8builtins17assumeNonNegativeyBwBwF
func assumeNonNegative(_ x: Builtin.Word) -> Builtin.Word {
  return Builtin.assumeNonNegative_Word(x)
  // CHECK: [[APPLY:%.*]] = builtin "assumeNonNegative_Word"(%0 : $Builtin.Word) : $Builtin.Word
  // CHECK: return [[APPLY]] : $Builtin.Word
}

// CHECK-LABEL: sil hidden @$s8builtins11autoreleaseyyAA1OCF : $@convention(thin) (@guaranteed O) -> () {
// ==> SEMANTIC ARC TODO: This will be unbalanced... should we allow it?
// CHECK: bb0([[ARG:%.*]] : @guaranteed $O):
// CHECK:   unmanaged_autorelease_value [[ARG]]
// CHECK: } // end sil function '$s8builtins11autoreleaseyyAA1OCF'
func autorelease(_ o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil hidden @$s8builtins11unreachable{{[_0-9a-zA-Z]*}}F
// CHECK:         builtin "unreachable"()
// CHECK:         return
// CANONICAL-LABEL: sil hidden @$s8builtins11unreachableyyF : $@convention(thin) () -> () {
func unreachable() {
  Builtin.unreachable()
}

// CHECK-LABEL: sil hidden @$s8builtins15reinterpretCast_1xBw_AA1DCAA1CCSgAGtAG_BwtF : $@convention(thin) (@guaranteed C, Builtin.Word) -> (Builtin.Word, @owned D, @owned Optional<C>, @owned C)
// CHECK:       bb0([[ARG1:%.*]] : @guaranteed $C, [[ARG2:%.*]] : @trivial $Builtin.Word):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[ARG1_COPY1:%.*]] = copy_value [[ARG1]]
// CHECK-NEXT:    [[ARG1_TRIVIAL:%.*]] = unchecked_trivial_bit_cast [[ARG1_COPY1]] : $C to $Builtin.Word
// CHECK-NEXT:    [[ARG1_COPY2:%.*]] = copy_value [[ARG1]]
// CHECK-NEXT:    [[ARG1_D:%.*]] = unchecked_ref_cast [[ARG1_COPY2]] : $C to $D
// CHECK-NEXT:    [[ARG1_COPY3:%.*]] = copy_value [[ARG1]]
// CHECK-NEXT:    [[ARG1_OPT:%.*]] = unchecked_ref_cast [[ARG1_COPY3]] : $C to $Optional<C>
// CHECK-NEXT:    [[ARG2_FROM_WORD:%.*]] = unchecked_bitwise_cast [[ARG2]] : $Builtin.Word to $C
// CHECK-NEXT:    [[ARG2_FROM_WORD_COPY:%.*]] = copy_value [[ARG2_FROM_WORD]]
// CHECK-NEXT:    destroy_value [[ARG1_COPY1]]
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ([[ARG1_TRIVIAL]] : $Builtin.Word, [[ARG1_D]] : $D, [[ARG1_OPT]] : $Optional<C>, [[ARG2_FROM_WORD_COPY:%.*]] : $C)
// CHECK:         return [[RESULT]]
func reinterpretCast(_ c: C, x: Builtin.Word) -> (Builtin.Word, D, C?, C) {
  return (Builtin.reinterpretCast(c) as Builtin.Word,
          Builtin.reinterpretCast(c) as D,
          Builtin.reinterpretCast(c) as C?,
          Builtin.reinterpretCast(x) as C)
}

// CHECK-LABEL: sil hidden @$s8builtins19reinterpretAddrOnly{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnly<T, U>(_ t: T) -> U {
  // CHECK: unchecked_addr_cast {{%.*}} : $*T to $*U
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @$s8builtins28reinterpretAddrOnlyToTrivial{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnlyToTrivial<T>(_ t: T) -> Int {
  // CHECK: copy_addr %0 to [initialization] [[INPUT:%.*]] : $*T
  // CHECK: [[ADDR:%.*]] = unchecked_addr_cast [[INPUT]] : $*T to $*Int
  // CHECK: [[VALUE:%.*]] = load [trivial] [[ADDR]]
  // CHECK: destroy_addr [[INPUT]]
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @$s8builtins27reinterpretAddrOnlyLoadable{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnlyLoadable<T>(_ a: Int, _ b: T) -> (T, Int) {
  // CHECK: [[BUF:%.*]] = alloc_stack $Int
  // CHECK: store {{%.*}} to [trivial] [[BUF]]
  // CHECK: [[RES1:%.*]] = unchecked_addr_cast [[BUF]] : $*Int to $*T
  // CHECK: copy_addr [[RES1]] to [initialization]
  return (Builtin.reinterpretCast(a) as T,
  // CHECK: [[RES:%.*]] = unchecked_addr_cast {{%.*}} : $*T to $*Int
  // CHECK: load [trivial] [[RES]]
          Builtin.reinterpretCast(b) as Int)
}

// CHECK-LABEL: sil hidden @$s8builtins18castToBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         [[ARG_COPY:%.*]] = copy_value %0
// CHECK:         [[BO:%.*]] = ref_to_bridge_object [[ARG_COPY]] : $C, {{%.*}} : $Builtin.Word
// CHECK-NOT:     destroy_value [[ARG_COPY]]
// CHECK:         return [[BO]]
func castToBridgeObject(_ c: C, _ w: Builtin.Word) -> Builtin.BridgeObject {
  return Builtin.castToBridgeObject(c, w)
}

// CHECK-LABEL: sil hidden @$s8builtins23castRefFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_ref [[BO:%.*]] : $Builtin.BridgeObject to $C
func castRefFromBridgeObject(_ bo: Builtin.BridgeObject) -> C {
  return Builtin.castReferenceFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @$s8builtins30castBitPatternFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_word [[BO:%.*]] : $Builtin.BridgeObject to $Builtin.Word
// CHECK-NOT:         destroy_value [[BO]]
func castBitPatternFromBridgeObject(_ bo: Builtin.BridgeObject) -> Builtin.Word {
  return Builtin.castBitPatternFromBridgeObject(bo)
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// NativeObject
// CHECK-LABEL: sil hidden @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Optional<Builtin.NativeObject>):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Optional<Builtin.NativeObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject nonNull
// CHECK-LABEL: sil hidden @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Builtin.NativeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Builtin.NativeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC)
// CHECK-LABEL: sil hidden @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Optional<Builtin.UnknownObject>):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Optional<Builtin.UnknownObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.UnknownObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) nonNull
// CHECK-LABEL: sil hidden @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Builtin.UnknownObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Builtin.UnknownObject
// CHECK: return
func isUnique(_ ref: inout Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// BridgeObject nonNull
// CHECK-LABEL: sil hidden @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Builtin.BridgeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Builtin.BridgeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// BridgeObject nonNull native
// CHECK-LABEL: sil hidden @$s8builtins15isUnique_native{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : @trivial $*Builtin.BridgeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[WRITE]] : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: return
func isUnique_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique_native(&ref))
}

// ----------------------------------------------------------------------------
// Builtin.castReference
// ----------------------------------------------------------------------------

class A {}
protocol PUnknown {}
protocol PClass : class {}

// CHECK-LABEL: sil hidden @$s8builtins19refcast_generic_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr  T in %{{.*}} : $*T to AnyObject in %{{.*}} : $*AnyObject
func refcast_generic_any<T>(_ o: T) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @$s8builtins17refcast_class_anyyyXlAA1ACF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $A):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[ARG_CASTED:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $A to $AnyObject
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[ARG_CASTED]]
// CHECK: } // end sil function '$s8builtins17refcast_class_anyyyXlAA1ACF'
func refcast_class_any(_ o: A) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @$s8builtins20refcast_punknown_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr PUnknown in %{{.*}} : $*PUnknown to AnyObject in %{{.*}} : $*AnyObject
func refcast_punknown_any(_ o: PUnknown) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @$s8builtins18refcast_pclass_anyyyXlAA6PClass_pF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $PClass):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[ARG_CAST:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $PClass to $AnyObject
// CHECK:   return [[ARG_CAST]]
// CHECK: } // end sil function '$s8builtins18refcast_pclass_anyyyXlAA6PClass_pF'
func refcast_pclass_any(_ o: PClass) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @$s8builtins20refcast_any_punknown{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr AnyObject in %{{.*}} : $*AnyObject to PUnknown in %{{.*}} : $*PUnknown
func refcast_any_punknown(_ o: AnyObject) -> PUnknown {
  return Builtin.castReference(o)
}

// => SEMANTIC ARC TODO: This function is missing a borrow + extract + copy.
//
// CHECK-LABEL: sil hidden @$s8builtins22unsafeGuaranteed_class{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : @guaranteed $A):
// CHECK:   [[P_COPY:%.*]] = copy_value  [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<A>([[P_COPY]] : $A)
// CHECK:   ([[R:%.*]], [[K:%.*]]) = destructure_tuple [[T]]
// CHECK:   destroy_value [[R]]
// CHECK:   [[P_COPY:%.*]] = copy_value [[P]]
// CHECK:   return [[P_COPY]] : $A
// CHECK: }
func unsafeGuaranteed_class(_ a: A) -> A {
  Builtin.unsafeGuaranteed(a)
  return a
}

// CHECK-LABEL: $s8builtins24unsafeGuaranteed_generic{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : @guaranteed $T):
// CHECK:   [[P_COPY:%.*]] = copy_value  [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P_COPY]] : $T)
// CHECK:   ([[R:%.*]], [[K:%.*]]) = destructure_tuple [[T]]
// CHECK:   destroy_value [[R]]
// CHECK:   [[P_RETURN:%.*]] = copy_value [[P]]
// CHECK:   return [[P_RETURN]] : $T
// CHECK: }
func unsafeGuaranteed_generic<T: AnyObject> (_ a: T) -> T {
  Builtin.unsafeGuaranteed(a)
  return a
}

// CHECK_LABEL: sil hidden @$s8builtins31unsafeGuaranteed_generic_return{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : @guaranteed $T):
// CHECK:   [[P_COPY:%.*]] = copy_value [[P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P_COPY]] : $T)
// CHECK:   ([[R:%.*]], [[K:%.*]]) = destructure_tuple [[T]]
// CHECK:   [[S:%.*]] = tuple ([[R]] : $T, [[K]] : $Builtin.Int8)
// CHECK:   return [[S]] : $(T, Builtin.Int8)
// CHECK: }
func unsafeGuaranteed_generic_return<T: AnyObject> (_ a: T) -> (T, Builtin.Int8) {
  return Builtin.unsafeGuaranteed(a)
}

// CHECK-LABEL: sil hidden @$s8builtins19unsafeGuaranteedEnd{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : @trivial $Builtin.Int8):
// CHECK:   builtin "unsafeGuaranteedEnd"([[P]] : $Builtin.Int8)
// CHECK:   [[S:%.*]] = tuple ()
// CHECK:   return [[S]] : $()
// CHECK: }
func unsafeGuaranteedEnd(_ t: Builtin.Int8) {
  Builtin.unsafeGuaranteedEnd(t)
}

// CHECK-LABEL: sil hidden @$s8builtins10bindMemory{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : @trivial $Builtin.RawPointer, [[I:%.*]] : @trivial $Builtin.Word, [[T:%.*]] : @trivial $@thick T.Type):
// CHECK: bind_memory [[P]] : $Builtin.RawPointer, [[I]] : $Builtin.Word to $*T
// CHECK:   return {{%.*}} : $()
// CHECK: }
func bindMemory<T>(ptr: Builtin.RawPointer, idx: Builtin.Word, _: T.Type) {
  Builtin.bindMemory(ptr, idx, T.self)
}

//===----------------------------------------------------------------------===//
// RC Operations
//===----------------------------------------------------------------------===//

// SILGen test:
//
// CHECK-LABEL: sil hidden @$s8builtins6retain{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
// CHECK: bb0([[P:%.*]] : @guaranteed $Builtin.NativeObject):
// CHECK:   unmanaged_retain_value [[P]]
// CHECK: } // end sil function '$s8builtins6retain{{[_0-9a-zA-Z]*}}F'

// SIL Test. This makes sure that we properly clean up in -Onone SIL.
// CANONICAL-LABEL: sil hidden @$s8builtins6retain{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
// CANONICAL: bb0([[P:%.*]] : $Builtin.NativeObject):
// CANONICAL: strong_retain [[P]]
// CANONICAL-NOT: retain
// CANONICAL-NOT: release
// CANONICAL: } // end sil function '$s8builtins6retain{{[_0-9a-zA-Z]*}}F'
func retain(ptr: Builtin.NativeObject) {
  Builtin.retain(ptr)
}

// SILGen test:
//
// CHECK-LABEL: sil hidden @$s8builtins7release{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
// CHECK: bb0([[P:%.*]] : @guaranteed $Builtin.NativeObject):
// CHECK:   unmanaged_release_value [[P]]
// CHECK-NOT:   destroy_value [[P]]
// CHECK: } // end sil function '$s8builtins7release{{[_0-9a-zA-Z]*}}F'

// SIL Test. Make sure even in -Onone code, we clean this up properly:
// CANONICAL-LABEL: sil hidden @$s8builtins7release{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
// CANONICAL: bb0([[P:%.*]] : $Builtin.NativeObject):
// CANONICAL-NEXT:   debug_value
// CANONICAL-NEXT:   strong_release [[P]]
// CANONICAL-NEXT:   tuple
// CANONICAL-NEXT:   tuple
// CANONICAL-NEXT:   return
// CANONICAL: } // end sil function '$s8builtins7release{{[_0-9a-zA-Z]*}}F'

func release(ptr: Builtin.NativeObject) {
  Builtin.release(ptr)
}

//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//

func once_helper() {}

// CHECK-LABEL: sil hidden @$s8builtins4once7controlyBp_tF
// CHECK:      [[T0:%.*]] = function_ref @$s8builtins11once_helperyyFTo : $@convention(c) () -> ()
// CHECK-NEXT: builtin "once"(%0 : $Builtin.RawPointer, [[T0]] : $@convention(c) () -> ())
func once(control: Builtin.RawPointer) {
  Builtin.once(control, once_helper)
}


// CHECK-LABEL: sil hidden @$s8builtins19valueToBridgeObjectyBbSuF : $@convention(thin) (UInt) -> @owned Builtin.BridgeObject {
// CHECK: bb0([[UINT:%.*]] : @trivial $UInt):
// CHECK:   [[BI:%.*]] = struct_extract [[UINT]] : $UInt, #UInt._value
// CHECK:   [[CAST:%.*]] = value_to_bridge_object [[BI]]
// CHECK:   [[RET:%.*]] = copy_value [[CAST]] : $Builtin.BridgeObject
// CHECK:   return [[RET]] : $Builtin.BridgeObject
// CHECK: } // end sil function '$s8builtins19valueToBridgeObjectyBbSuF'
func valueToBridgeObject(_ x: UInt) -> Builtin.BridgeObject {
  return Builtin.valueToBridgeObject(x._value)
}

// CHECK-LABEL: sil hidden @$s8builtins10assumeTrueyyBi1_F
// CHECK: builtin "assume_Int1"({{.*}} : $Builtin.Int1)
// CHECK: return
func assumeTrue(_ x: Builtin.Int1) {
  Builtin.assume_Int1(x)
}

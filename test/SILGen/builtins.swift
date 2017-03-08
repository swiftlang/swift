// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -parse-stdlib %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil -Onone -parse-stdlib %s -disable-objc-attr-requires-foundation-module | %FileCheck -check-prefix=CANONICAL %s

import Swift

protocol ClassProto : class { }

struct Pointer {
  var value: Builtin.RawPointer
}

// CHECK-LABEL: sil hidden @_T08builtins3foo{{[_0-9a-zA-Z]*}}F
func foo(_ x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil hidden @_T08builtins8load_pod{{[_0-9a-zA-Z]*}}F
func load_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_T08builtins8load_obj{{[_0-9a-zA-Z]*}}F
func load_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_T08builtins12load_raw_pod{{[_0-9a-zA-Z]*}}F
func load_raw_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @_T08builtins12load_raw_obj{{[_0-9a-zA-Z]*}}F
func load_raw_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden @_T08builtins8load_gen{{[_0-9a-zA-Z]*}}F
func load_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[ADDR]] to [initialization] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden @_T08builtins8move_pod{{[_0-9a-zA-Z]*}}F
func move_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_T08builtins8move_obj{{[_0-9a-zA-Z]*}}F
func move_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [take] [[ADDR]]
  // CHECK-NOT: copy_value [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_T08builtins8move_gen{{[_0-9a-zA-Z]*}}F
func move_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] [[ADDR]] to [initialization] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden @_T08builtins11destroy_pod{{[_0-9a-zA-Z]*}}F
func destroy_pod(_ x: Builtin.RawPointer) {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box
  // CHECK-NOT: pointer_to_address
  // CHECK-NOT: destroy_addr
  // CHECK-NOT: destroy_value
  // CHECK: destroy_value [[XBOX]] : ${{.*}}{
  // CHECK-NOT: destroy_value
  return Builtin.destroy(Builtin.Int64, x)
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_T08builtins11destroy_obj{{[_0-9a-zA-Z]*}}F
func destroy_obj(_ x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject, x)
}

// CHECK-LABEL: sil hidden @_T08builtins11destroy_gen{{[_0-9a-zA-Z]*}}F
func destroy_gen<T>(_ x: Builtin.RawPointer, _: T) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil hidden @_T08builtins10assign_pod{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins10assign_obj{{[_0-9a-zA-Z]*}}F
func assign_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK: destroy_value
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_T08builtins12assign_tuple{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins10assign_gen{{[_0-9a-zA-Z]*}}F
func assign_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden @_T08builtins8init_pod{{[_0-9a-zA-Z]*}}F
func init_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [trivial] [[ADDR]]
  // CHECK-NOT: destroy_value [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_T08builtins8init_obj{{[_0-9a-zA-Z]*}}F
func init_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [init] [[ADDR]]
  // CHECK-NOT: destroy_value [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden @_T08builtins8init_gen{{[_0-9a-zA-Z]*}}F
func init_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[OTHER_LOC:%.*]] to [initialization]  [[ADDR]]
  // CHECK: destroy_addr [[OTHER_LOC]]
  Builtin.initialize(x, y)
}

class C {}
class D {}

// CHECK-LABEL: sil hidden @_T08builtins22class_to_native_object{{[_0-9a-zA-Z]*}}F
func class_to_native_object(_ c:C) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(c)
}

// CHECK-LABEL: sil hidden @_T08builtins23class_to_unknown_object{{[_0-9a-zA-Z]*}}F
func class_to_unknown_object(_ c:C) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(c)
}

// CHECK-LABEL: sil hidden @_T08builtins32class_archetype_to_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_to_native_object<T : C>(_ t: T) -> Builtin.NativeObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.NativeObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_T08builtins33class_archetype_to_unknown_object{{[_0-9a-zA-Z]*}}F
func class_archetype_to_unknown_object<T : C>(_ t: T) -> Builtin.UnknownObject {
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[C:%.*]] to $Builtin.UnknownObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_T08builtins34class_existential_to_native_object{{[_0-9a-zA-Z]*}}F
func class_existential_to_native_object(_ t:ClassProto) -> Builtin.NativeObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.NativeObject
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden @_T08builtins35class_existential_to_unknown_object{{[_0-9a-zA-Z]*}}F
func class_existential_to_unknown_object(_ t:ClassProto) -> Builtin.UnknownObject {
  // CHECK: [[REF:%[0-9]+]] = open_existential_ref [[T:%[0-9]+]] : $ClassProto
  // CHECK: [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}) ClassProto to $Builtin.UnknownObject
  return Builtin.castToUnknownObject(t)
}

// CHECK-LABEL: sil hidden @_T08builtins24class_from_native_object{{[_0-9a-zA-Z]*}}F
func class_from_native_object(_ p: Builtin.NativeObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins25class_from_unknown_object{{[_0-9a-zA-Z]*}}F
func class_from_unknown_object(_ p: Builtin.UnknownObject) -> C {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] to $C
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins34class_archetype_from_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_from_native_object<T : C>(_ p: Builtin.NativeObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $T
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins35class_archetype_from_unknown_object{{[_0-9a-zA-Z]*}}F
func class_archetype_from_unknown_object<T : C>(_ p: Builtin.UnknownObject) -> T {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $T
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins41objc_class_existential_from_native_object{{[_0-9a-zA-Z]*}}F
func objc_class_existential_from_native_object(_ p: Builtin.NativeObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.NativeObject to $AnyObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins42objc_class_existential_from_unknown_object{{[_0-9a-zA-Z]*}}F
func objc_class_existential_from_unknown_object(_ p: Builtin.UnknownObject) -> AnyObject {
  // CHECK: [[C:%.*]] = unchecked_ref_cast [[OBJ:%.*]] : $Builtin.UnknownObject to $AnyObject
  // CHECK-NOT: destroy_value [[C]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[C]]
  return Builtin.castFromUnknownObject(p)
}

// CHECK-LABEL: sil hidden @_T08builtins20class_to_raw_pointer{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins18obj_to_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_to_raw_pointer(_ c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil hidden @_T08builtins22class_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func class_from_raw_pointer(_ p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(_ p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_T08builtins20obj_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_T08builtins28unknown_obj_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func unknown_obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.UnknownObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.UnknownObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_T08builtins28existential_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func existential_from_raw_pointer(_ p: Builtin.RawPointer) -> AnyObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $AnyObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden @_T08builtins9gep_raw64{{[_0-9a-zA-Z]*}}F
func gep_raw64(_ p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int64(p, i)
}

// CHECK-LABEL: sil hidden @_T08builtins9gep_raw32{{[_0-9a-zA-Z]*}}F
func gep_raw32(_ p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int32(p, i)
}

// CHECK-LABEL: sil hidden @_T08builtins3gep{{[_0-9a-zA-Z]*}}F
func gep<Elem>(_ p: Builtin.RawPointer, i: Builtin.Word, e: Elem.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[GEP:%.*]] = index_addr [[P2A]] : $*Elem, %1 : $Builtin.Word
  // CHECK: [[A2P:%.*]] = address_to_pointer [[GEP]]
  // CHECK: return [[A2P]]
  return Builtin.gep_Word(p, i, e)
}

public final class Header { }

// CHECK-LABEL: sil hidden @_T08builtins20allocWithTailElems_1{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_1<T>(n: Builtin.Word, ty: T.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T * %0 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_1(Header.self, n, ty)
}

// CHECK-LABEL: sil hidden @_T08builtins20allocWithTailElems_3{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_3<T1, T2, T3>(n1: Builtin.Word, ty1: T1.Type, n2: Builtin.Word, ty2: T2.Type, n3: Builtin.Word, ty3: T3.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T1 * %0 : $Builtin.Word] [tail_elems $T2 * %2 : $Builtin.Word] [tail_elems $T3 * %4 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_3(Header.self, n1, ty1, n2, ty2, n3, ty3)
}

// CHECK-LABEL: sil hidden @_T08builtins16projectTailElems{{[_0-9a-zA-Z]*}}F
func projectTailElems<T>(h: Header, ty: T.Type) -> Builtin.RawPointer {
  // CHECK: bb0([[ARG1:%.*]] : $Header
  // CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
  // CHECK:   [[ARG1_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
  // CHECK:   [[TA:%.*]] = ref_tail_addr [[ARG1_COPY]] : $Header
  // CHECK:   [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK:   destroy_value [[ARG1_COPY]]
  // CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG1]]
  // CHECK:   destroy_value [[ARG1]]
  // CHECK:   return [[A2P]]
  return Builtin.projectTailElems(h, ty)
}
// CHECK: } // end sil function '_T08builtins16projectTailElemsBpAA6HeaderC1h_xm2tytlF'

// CHECK-LABEL: sil hidden @_T08builtins11getTailAddr{{[_0-9a-zA-Z]*}}F
func getTailAddr<T1, T2>(start: Builtin.RawPointer, i: Builtin.Word, ty1: T1.Type, ty2: T2.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[TA:%.*]] = tail_addr [[P2A]] : $*T1, %1 : $Builtin.Word, $T2
  // CHECK: [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK: return [[A2P]]
  return Builtin.getTailAddr_Word(start, i, ty1, ty2)
}

// CHECK-LABEL: sil hidden @_T08builtins8condfail{{[_0-9a-zA-Z]*}}F
func condfail(_ i: Builtin.Int1) {
  Builtin.condfail(i)
  // CHECK: cond_fail {{%.*}} : $Builtin.Int1
}

struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: sil hidden @_T08builtins10canBeClass{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins18canBeClassMetatype{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins11fixLifetimeyAA1CCF : $@convention(thin) (@owned C) -> () {
func fixLifetime(_ c: C) {
  // CHECK: bb0([[ARG:%.*]] : $C):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   fix_lifetime [[ARG_COPY]] : $C
  // CHECK:   destroy_value [[ARG_COPY]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   destroy_value [[ARG]]
  Builtin.fixLifetime(c)
}
// CHECK: } // end sil function '_T08builtins11fixLifetimeyAA1CCF'

// CHECK-LABEL: sil hidden @_T08builtins20assert_configuration{{[_0-9a-zA-Z]*}}F
func assert_configuration() -> Builtin.Int32 {
  return Builtin.assert_configuration()
  // CHECK: [[APPLY:%.*]] = builtin "assert_configuration"() : $Builtin.Int32
  // CHECK: return [[APPLY]] : $Builtin.Int32
}

// CHECK-LABEL: sil hidden @_T08builtins17assumeNonNegativeBwBwF
func assumeNonNegative(_ x: Builtin.Word) -> Builtin.Word {
  return Builtin.assumeNonNegative_Word(x)
  // CHECK: [[APPLY:%.*]] = builtin "assumeNonNegative_Word"(%0 : $Builtin.Word) : $Builtin.Word
  // CHECK: return [[APPLY]] : $Builtin.Word
}

// CHECK-LABEL: sil hidden @_T08builtins11autoreleaseyAA1OCF : $@convention(thin) (@owned O) -> () {
// ==> SEMANTIC ARC TODO: This will be unbalanced... should we allow it?
// CHECK: bb0([[ARG:%.*]] : $O):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   unmanaged_autorelease_value [[ARG_COPY]]
// CHECK:   destroy_value [[ARG_COPY]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_T08builtins11autoreleaseyAA1OCF'
func autorelease(_ o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil hidden @_T08builtins11unreachable{{[_0-9a-zA-Z]*}}F
// CHECK:         builtin "unreachable"()
// CHECK:         return
// CANONICAL-LABEL: sil hidden @_T08builtins11unreachableyyF : $@convention(thin) () -> () {
func unreachable() {
  Builtin.unreachable()
}

// CHECK-LABEL: sil hidden @_T08builtins15reinterpretCastBw_AA1DCAA1CCSgAFtAF_Bw1xtF : $@convention(thin) (@owned C, Builtin.Word) -> (Builtin.Word, @owned D, @owned Optional<C>, @owned C)
// CHECK:       bb0([[ARG1:%.*]] : $C, [[ARG2:%.*]] : $Builtin.Word):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[BORROWED_ARG1_1:%.*]] = begin_borrow [[ARG1]]
// CHECK-NEXT:    [[ARG1_COPY1:%.*]] = copy_value [[BORROWED_ARG1_1]] : $C
// CHECK-NEXT:    [[ARG1_TRIVIAL:%.*]] = unchecked_trivial_bit_cast [[ARG1_COPY1]] : $C to $Builtin.Word
// CHECK-NEXT:    [[BORROWED_ARG1_2:%.*]] = begin_borrow [[ARG1]]
// CHECK-NEXT:    [[ARG1_COPY2:%.*]] = copy_value [[BORROWED_ARG1_2]] : $C
// CHECK-NEXT:    [[ARG1_COPY2_CASTED:%.*]] = unchecked_ref_cast [[ARG1_COPY2]] : $C to $D
// CHECK-NEXT:    [[BORROWED_ARG1_3:%.*]] = begin_borrow [[ARG1]]
// CHECK-NEXT:    [[ARG1_COPY3:%.*]] = copy_value [[BORROWED_ARG1_3]]
// CHECK-NEXT:    [[ARG1_COPY3_CAST:%.*]] = unchecked_ref_cast [[ARG1_COPY3]] : $C to $Optional<C>
// CHECK-NEXT:    [[ARG2_OBJ_CASTED:%.*]] = unchecked_bitwise_cast [[ARG2]] : $Builtin.Word to $C
// CHECK-NEXT:    [[ARG2_OBJ_CASTED_COPIED:%.*]] = copy_value [[ARG2_OBJ_CASTED]] : $C
// CHECK-NEXT:    end_borrow [[BORROWED_ARG1_3]] from [[ARG1]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG1_2]] from [[ARG1]]
// CHECK-NEXT:    destroy_value [[ARG1_COPY1]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG1_1]] from [[ARG1]]
// CHECK-NEXT:    destroy_value [[ARG1]]
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ([[ARG1_TRIVIAL]] : $Builtin.Word, [[ARG1_COPY2_CASTED]] : $D, [[ARG1_COPY3_CAST]] : $Optional<C>, [[ARG2_OBJ_CASTED_COPIED:%.*]] : $C)
// CHECK:         return [[RESULT]]
func reinterpretCast(_ c: C, x: Builtin.Word) -> (Builtin.Word, D, C?, C) {
  return (Builtin.reinterpretCast(c) as Builtin.Word,
          Builtin.reinterpretCast(c) as D,
          Builtin.reinterpretCast(c) as C?,
          Builtin.reinterpretCast(x) as C)
}

// CHECK-LABEL: sil hidden @_T08builtins19reinterpretAddrOnly{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnly<T, U>(_ t: T) -> U {
  // CHECK: unchecked_addr_cast {{%.*}} : $*T to $*U
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_T08builtins28reinterpretAddrOnlyToTrivial{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnlyToTrivial<T>(_ t: T) -> Int {
  // CHECK: [[ADDR:%.*]] = unchecked_addr_cast [[INPUT:%.*]] : $*T to $*Int
  // CHECK: [[VALUE:%.*]] = load [trivial] [[ADDR]]
  // CHECK: destroy_addr [[INPUT]]
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden @_T08builtins27reinterpretAddrOnlyLoadable{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T08builtins18castToBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         [[BO:%.*]] = ref_to_bridge_object {{%.*}} : $C, {{%.*}} : $Builtin.Word
// CHECK:         return [[BO]]
func castToBridgeObject(_ c: C, _ w: Builtin.Word) -> Builtin.BridgeObject {
  return Builtin.castToBridgeObject(c, w)
}

// CHECK-LABEL: sil hidden @_T08builtins23castRefFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_ref [[BO:%.*]] : $Builtin.BridgeObject to $C
func castRefFromBridgeObject(_ bo: Builtin.BridgeObject) -> C {
  return Builtin.castReferenceFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_T08builtins30castBitPatternFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_word [[BO:%.*]] : $Builtin.BridgeObject to $Builtin.Word
// CHECK:         destroy_value [[BO]]
func castBitPatternFromBridgeObject(_ bo: Builtin.BridgeObject) -> Builtin.Word {
  return Builtin.castBitPatternFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden @_T08builtins8pinUnpin{{[_0-9a-zA-Z]*}}F
// CHECK:       bb0([[ARG:%.*]] : $Builtin.NativeObject):
// CHECK-NEXT:    debug_value
func pinUnpin(_ object : Builtin.NativeObject) {
// CHECK-NEXT:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT:    [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]] : $Builtin.NativeObject
// CHECK-NEXT:    [[HANDLE:%.*]] = strong_pin [[ARG_COPY]] : $Builtin.NativeObject
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    destroy_value [[ARG_COPY]] : $Builtin.NativeObject
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
  let handle : Builtin.NativeObject? = Builtin.tryPin(object)

// CHECK-NEXT:    [[BORROWED_HANDLE:%.*]] = begin_borrow [[HANDLE]]
// CHECK-NEXT:    [[HANDLE_COPY:%.*]] = copy_value [[BORROWED_HANDLE]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:    strong_unpin [[HANDLE_COPY]] : $Optional<Builtin.NativeObject>
// ==> SEMANTIC ARC TODO: This looks like a mispairing or a weird pairing.
  Builtin.unpin(handle)

// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    end_borrow [[BORROWED_HANDLE]] from [[HANDLE]]
// CHECK-NEXT:    destroy_value [[HANDLE]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:    destroy_value [[ARG]] : $Builtin.NativeObject
// CHECK-NEXT:    [[T0:%.*]] = tuple ()
// CHECK-NEXT:    return [[T0]] : $()
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// NativeObject
// CHECK-LABEL: sil hidden @_T08builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK: [[BUILTIN:%.*]] = is_unique %0 : $*Optional<Builtin.NativeObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject nonNull
// CHECK-LABEL: sil hidden @_T08builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK: [[BUILTIN:%.*]] = is_unique %0 : $*Builtin.NativeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// NativeObject pinned
// CHECK-LABEL: sil hidden @_T08builtins16isUniqueOrPinned{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned %0 : $*Optional<Builtin.NativeObject>
// CHECK: return
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject?) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// NativeObject pinned nonNull
// CHECK-LABEL: sil hidden @_T08builtins16isUniqueOrPinned{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned %0 : $*Builtin.NativeObject
// CHECK: return
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// UnknownObject (ObjC)
// CHECK-LABEL: sil hidden @_T08builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Optional<Builtin.UnknownObject>):
// CHECK: [[BUILTIN:%.*]] = is_unique %0 : $*Optional<Builtin.UnknownObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.UnknownObject?) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) nonNull
// CHECK-LABEL: sil hidden @_T08builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK: [[BUILTIN:%.*]] = is_unique %0 : $*Builtin.UnknownObject
// CHECK: return
func isUnique(_ ref: inout Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// UnknownObject (ObjC) pinned nonNull
// CHECK-LABEL: sil hidden @_T08builtins16isUniqueOrPinned{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.UnknownObject):
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned %0 : $*Builtin.UnknownObject
// CHECK: return
func isUniqueOrPinned(_ ref: inout Builtin.UnknownObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull
// CHECK-LABEL: sil hidden @_T08builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[BUILTIN:%.*]] = is_unique %0 : $*Builtin.BridgeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique(&ref))
}

// BridgeObject pinned nonNull
// CHECK-LABEL: sil hidden @_T08builtins16isUniqueOrPinned{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned %0 : $*Builtin.BridgeObject
// CHECK: return
func isUniqueOrPinned(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned(&ref))
}

// BridgeObject nonNull native
// CHECK-LABEL: sil hidden @_T08builtins15isUnique_native{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[CAST:%.*]] = unchecked_addr_cast %0 : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: return
func isUnique_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUnique_native(&ref))
}

// BridgeObject pinned nonNull native
// CHECK-LABEL: sil hidden @_T08builtins23isUniqueOrPinned_native{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[CAST:%.*]] = unchecked_addr_cast %0 : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: [[BUILTIN:%.*]] = is_unique_or_pinned [[CAST]] : $*Builtin.NativeObject
// CHECK: return
func isUniqueOrPinned_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return _getBool(Builtin.isUniqueOrPinned_native(&ref))
}

// ----------------------------------------------------------------------------
// Builtin.castReference
// ----------------------------------------------------------------------------

class A {}
protocol PUnknown {}
protocol PClass : class {}

// CHECK-LABEL: sil hidden @_T08builtins19refcast_generic_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr  T in %{{.*}} : $*T to AnyObject in %{{.*}} : $*AnyObject
func refcast_generic_any<T>(_ o: T) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_T08builtins17refcast_class_anys9AnyObject_pAA1ACF :
// CHECK: bb0([[ARG:%.*]] : $A):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[ARG_COPY_CASTED:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $A to $AnyObject
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[ARG_COPY_CASTED]]
// CHECK: } // end sil function '_T08builtins17refcast_class_anys9AnyObject_pAA1ACF'
func refcast_class_any(_ o: A) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_T08builtins20refcast_punknown_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr PUnknown in %{{.*}} : $*PUnknown to AnyObject in %{{.*}} : $*AnyObject
func refcast_punknown_any(_ o: PUnknown) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_T08builtins18refcast_pclass_anys9AnyObject_pAA6PClass_pF :
// CHECK: bb0([[ARG:%.*]] : $PClass):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[ARG_COPY_CAST:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $PClass to $AnyObject
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[ARG_COPY_CAST]]
// CHECK: } // end sil function '_T08builtins18refcast_pclass_anys9AnyObject_pAA6PClass_pF'
func refcast_pclass_any(_ o: PClass) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden @_T08builtins20refcast_any_punknown{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr AnyObject in %{{.*}} : $*AnyObject to PUnknown in %{{.*}} : $*PUnknown
func refcast_any_punknown(_ o: AnyObject) -> PUnknown {
  return Builtin.castReference(o)
}

// => SEMANTIC ARC TODO: This function is missing a borrow + extract + copy.
//
// CHECK-LABEL: sil hidden @_T08builtins22unsafeGuaranteed_class{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $A):
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[P_COPY:%.*]] = copy_value  [[BORROWED_P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<A>([[P_COPY]] : $A)
// CHECK:   [[BORROWED_T:%.*]] = begin_borrow [[T]]
// CHECK:   [[R:%.*]] = tuple_extract [[BORROWED_T]] : $(A, Builtin.Int8), 0
// CHECK:   [[COPY_R:%.*]] = copy_value [[R]]
// CHECK:   [[K:%.*]] = tuple_extract [[BORROWED_T]] : $(A, Builtin.Int8), 1
// CHECK:   destroy_value [[COPY_R]] : $A
// CHECK:   end_borrow [[BORROWED_T]] from [[T]]
// CHECK:   destroy_value [[T]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[P_COPY:%.*]] = copy_value [[BORROWED_P]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   destroy_value [[P]]
// CHECK:   return [[P_COPY]] : $A
// CHECK: }
func unsafeGuaranteed_class(_ a: A) -> A {
  Builtin.unsafeGuaranteed(a)
  return a
}

// => SEMANTIC ARC TODO: This function is missing a borrow + extract + copy.
//
// CHECK-LABEL: _T08builtins24unsafeGuaranteed_generic{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $T):
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[P_COPY:%.*]] = copy_value  [[BORROWED_P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P_COPY]] : $T)
// CHECK:   [[BORROWED_T:%.*]] = begin_borrow [[T]]
// CHECK:   [[R:%.*]] = tuple_extract [[BORROWED_T]] : $(T, Builtin.Int8), 0
// CHECK:   [[COPY_R:%.*]] = copy_value [[R]]
// CHECK:   [[K:%.*]] = tuple_extract [[BORROWED_T]] : $(T, Builtin.Int8), 1
// CHECK:   destroy_value [[COPY_R]] : $T
// CHECK:   end_borrow [[BORROWED_T]] from [[T]]
// CHECK:   destroy_value [[T]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[P_RETURN:%.*]] = copy_value [[BORROWED_P]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   destroy_value [[P]]
// CHECK:   return [[P_RETURN]] : $T
// CHECK: }
func unsafeGuaranteed_generic<T: AnyObject> (_ a: T) -> T {
  Builtin.unsafeGuaranteed(a)
  return a
}

// CHECK_LABEL: sil hidden @_T08builtins31unsafeGuaranteed_generic_return{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $T):
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[P_COPY:%.*]] = copy_value [[BORROWED_P]]
// CHECK:   [[T:%.*]] = builtin "unsafeGuaranteed"<T>([[P_COPY]] : $T)
// CHECK:   [[BORROWED_T:%.*]] = begin_borrow [[T]]
// CHECK:   [[R]] = tuple_extract [[BORROWED_T]] : $(T, Builtin.Int8), 0
// CHECK:   [[COPY_R:%.*]] = copy_value [[R]]
// CHECK:   [[K]] = tuple_extract [[BORROWED_T]] : $(T, Builtin.Int8), 1
// CHECK:   end_borrow [[BORROWED_T]] from [[T]]
// CHECK:   destroy_value [[T]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   destroy_value [[P]]
// CHECK:   [[S:%.*]] = tuple ([[COPY_R]] : $T, [[K]] : $Builtin.Int8)
// CHECK:   return [[S]] : $(T, Builtin.Int8)
// CHECK: }
func unsafeGuaranteed_generic_return<T: AnyObject> (_ a: T) -> (T, Builtin.Int8) {
  return Builtin.unsafeGuaranteed(a)
}

// CHECK-LABEL: sil hidden @_T08builtins19unsafeGuaranteedEnd{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $Builtin.Int8):
// CHECK:   builtin "unsafeGuaranteedEnd"([[P]] : $Builtin.Int8)
// CHECK:   [[S:%.*]] = tuple ()
// CHECK:   return [[S]] : $()
// CHECK: }
func unsafeGuaranteedEnd(_ t: Builtin.Int8) {
  Builtin.unsafeGuaranteedEnd(t)
}

// CHECK-LABEL: sil hidden @_T08builtins10bindMemory{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $Builtin.RawPointer, [[I:%.*]] : $Builtin.Word, [[T:%.*]] : $@thick T.Type):
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
// CHECK-LABEL: sil hidden @_T08builtins6retain{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned Builtin.NativeObject) -> () {
// CHECK: bb0([[P:%.*]] : $Builtin.NativeObject):
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[COPIED_P:%.*]] = copy_value [[BORROWED_P]]
// CHECK:   unmanaged_retain_value [[COPIED_P]]
// CHECK:   destroy_value [[COPIED_P]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   destroy_value [[P]]
// CHECK: } // end sil function '_T08builtins6retain{{[_0-9a-zA-Z]*}}F'

// SIL Test. This makes sure that we properly clean up in -Onone SIL.
// CANONICAL-LABEL: sil hidden @_T08builtins6retain{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned Builtin.NativeObject) -> () {
// CANONICAL: bb0([[P:%.*]] : $Builtin.NativeObject):
// CANONICAL-NOT: retain
// CANONICAL-NOT: release
// CANONICAL: } // end sil function '_T08builtins6retain{{[_0-9a-zA-Z]*}}F'
func retain(ptr: Builtin.NativeObject) {
  Builtin.retain(ptr)
}

// SILGen test:
//
// CHECK-LABEL: sil hidden @_T08builtins7release{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned Builtin.NativeObject) -> () {
// CHECK: bb0([[P:%.*]] : $Builtin.NativeObject):
// CHECK:   [[BORROWED_P:%.*]] = begin_borrow [[P]]
// CHECK:   [[COPIED_P:%.*]] = copy_value [[BORROWED_P]]
// CHECK:   unmanaged_release_value [[COPIED_P]]
// CHECK:   destroy_value [[COPIED_P]]
// CHECK:   end_borrow [[BORROWED_P]] from [[P]]
// CHECK:   destroy_value [[P]]
// CHECK: } // end sil function '_T08builtins7release{{[_0-9a-zA-Z]*}}F'

// SIL Test. Make sure even in -Onone code, we clean this up properly:
// CANONICAL-LABEL: sil hidden @_T08builtins7release{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned Builtin.NativeObject) -> () {
// CANONICAL: bb0([[P:%.*]] : $Builtin.NativeObject):
// CANONICAL-NEXT:   debug_value
// CANONICAL-NEXT:   tuple
// CANONICAL-NEXT:   strong_release [[P]]
// CANONICAL-NEXT:   strong_release [[P]]
// CANONICAL-NEXT:   tuple
// CANONICAL-NEXT:   return
// CANONICAL: } // end sil function '_T08builtins7release{{[_0-9a-zA-Z]*}}F'

func release(ptr: Builtin.NativeObject) {
  Builtin.release(ptr)
}

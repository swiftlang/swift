// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types  -enable-builtin-module %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types  -enable-builtin-module -Onone %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=CANONICAL %s

// REQUIRES: swift_in_compiler

import Builtin

protocol ClassProto : class { }

struct Pointer {
  var value: Builtin.RawPointer
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins3foo{{[_0-9a-zA-Z]*}}F
func foo(_ x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  // CHECK: builtin "cmp_eq_Int1"
  return Builtin.cmp_eq_Int1(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8load_pod{{[_0-9a-zA-Z]*}}F
func load_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8load_obj{{[_0-9a-zA-Z]*}}F
func load_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins12load_raw_pod{{[_0-9a-zA-Z]*}}F
func load_raw_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [align=1] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins12load_raw_obj{{[_0-9a-zA-Z]*}}F
func load_raw_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [align=1] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadRaw(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18load_invariant_pod{{[_0-9a-zA-Z]*}}F
func load_invariant_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [invariant] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadInvariant(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18load_invariant_obj{{[_0-9a-zA-Z]*}}F
func load_invariant_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [invariant] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [copy] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.loadInvariant(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8load_gen{{[_0-9a-zA-Z]*}}F
func load_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[ADDR]] to [init] {{%.*}}
  return Builtin.load(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8move_pod{{[_0-9a-zA-Z]*}}F
func move_pod(_ x: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK: [[VAL:%.*]] = load [trivial] [[ADDR]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8move_obj{{[_0-9a-zA-Z]*}}F
func move_obj(_ x: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: [[VAL:%.*]] = load [take] [[ADDR]]
  // CHECK-NOT: copy_value [[VAL]]
  // CHECK: return [[VAL]]
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8move_gen{{[_0-9a-zA-Z]*}}F
func move_gen<T>(_ x: Builtin.RawPointer) -> T {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] [[ADDR]] to [init] {{%.*}}
  return Builtin.take(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11destroy_pod{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11destroy_obj{{[_0-9a-zA-Z]*}}F
func destroy_obj(_ x: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(Builtin.NativeObject.self, x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11destroy_gen{{[_0-9a-zA-Z]*}}F
func destroy_gen<T>(_ x: Builtin.RawPointer, _: T) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: destroy_addr [[ADDR]]
  return Builtin.destroy(T.self, x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10assign_pod{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10assign_obj{{[_0-9a-zA-Z]*}}F
func assign_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK: assign {{%.*}} to [[ADDR]]
  // CHECK-NOT: destroy_value
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins12assign_tuple{{[_0-9a-zA-Z]*}}F
func assign_tuple(_ x: (Builtin.Int64, Builtin.NativeObject),
                  y: Builtin.RawPointer) {
  var x = x
  var y = y
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*(Builtin.Int64, Builtin.NativeObject)
  // CHECK: tuple_addr_constructor [assign] [[ADDR]] : $*(Builtin.Int64, Builtin.NativeObject) with
  // CHECK: destroy_value
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10assign_gen{{[_0-9a-zA-Z]*}}F
func assign_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [take] {{%.*}} to [[ADDR]] :
  Builtin.assign(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8init_pod{{[_0-9a-zA-Z]*}}F
func init_pod(_ x: Builtin.Int64, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.Int64
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store {{%.*}} to [trivial] [[ADDR]]
  // CHECK-NOT: destroy_value [[ADDR]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8init_obj{{[_0-9a-zA-Z]*}}F
func init_obj(_ x: Builtin.NativeObject, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*Builtin.NativeObject
  // CHECK-NOT: load [[ADDR]]
  // CHECK: store [[SRC:%.*]] to [init] [[ADDR]]
  // CHECK-NOT: destroy_value [[SRC]]
  Builtin.initialize(x, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8init_gen{{[_0-9a-zA-Z]*}}F
func init_gen<T>(_ x: T, y: Builtin.RawPointer) {
  // CHECK: [[ADDR:%.*]] = pointer_to_address {{%.*}} to [strict] $*T
  // CHECK: copy_addr [[OTHER_LOC:%.*]] to [init]  [[ADDR]]
  // CHECK-NOT: destroy_addr [[OTHER_LOC]]
  Builtin.initialize(x, y)
}

class C {}
class D {}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins22class_to_native_object{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $C):
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT:   [[OBJ:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $C to $Builtin.NativeObject
// CHECK-NEXT:   return [[OBJ]]
func class_to_native_object(_ c:C) -> Builtin.NativeObject {
  return Builtin.castToNativeObject(c)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins32class_archetype_to_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_to_native_object<T : C>(_ t: T) -> Builtin.NativeObject {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[OBJ:%.*]] = unchecked_ref_cast [[COPY]] : $T to $Builtin.NativeObject
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[OBJ]]
  // CHECK: return [[OBJ]]
  return Builtin.castToNativeObject(t)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins34class_existential_to_native_object{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any ClassProto):
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT:   [[REF:%[0-9]+]] = open_existential_ref [[ARG_COPY]] : $any ClassProto
// CHECK-NEXT:   [[PTR:%[0-9]+]] = unchecked_ref_cast [[REF]] : $@opened({{.*}}, any ClassProto) Self to $Builtin.NativeObject
// CHECK-NEXT:   return [[PTR]]
func class_existential_to_native_object(_ t:ClassProto) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(t as ClassProto)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins24class_from_native_object{{[_0-9a-zA-Z]*}}F
func class_from_native_object(_ p: Builtin.NativeObject) -> C {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $C
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins34class_archetype_from_native_object{{[_0-9a-zA-Z]*}}F
func class_archetype_from_native_object<T : C>(_ p: Builtin.NativeObject) -> T {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $T
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins41objc_class_existential_from_native_object{{[_0-9a-zA-Z]*}}F
func objc_class_existential_from_native_object(_ p: Builtin.NativeObject) -> AnyObject {
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[CAST:%.*]] = unchecked_ref_cast [[COPY]] : $Builtin.NativeObject to $AnyObject
  // CHECK-NOT: destroy_value [[COPY]]
  // CHECK-NOT: destroy_value [[CAST]]
  // CHECK: return [[CAST]]
  return Builtin.castFromNativeObject(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20class_to_raw_pointer{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18obj_to_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_to_raw_pointer(_ c: Builtin.NativeObject) -> Builtin.RawPointer {
  // CHECK: [[RAW:%.*]] = ref_to_raw_pointer [[C:%.*]] to $Builtin.RawPointer
  // CHECK: return [[RAW]]
  return Builtin.bridgeToRawPointer(c)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins22class_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func class_from_raw_pointer(_ p: Builtin.RawPointer) -> C {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $C
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

func class_archetype_from_raw_pointer<T : C>(_ p: Builtin.RawPointer) -> T {
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20obj_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func obj_from_raw_pointer(_ p: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $Builtin.NativeObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins28existential_from_raw_pointer{{[_0-9a-zA-Z]*}}F
func existential_from_raw_pointer(_ p: Builtin.RawPointer) -> AnyObject {
  // CHECK: [[C:%.*]] = raw_pointer_to_ref [[RAW:%.*]] to $AnyObject
  // CHECK: [[C_COPY:%.*]] = copy_value [[C]]
  // CHECK: return [[C_COPY]]
  return Builtin.bridgeFromRawPointer(p)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins9gep_raw64{{[_0-9a-zA-Z]*}}F
func gep_raw64(_ p: Builtin.RawPointer, i: Builtin.Int64) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int64(p, i)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins9gep_raw32{{[_0-9a-zA-Z]*}}F
func gep_raw32(_ p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  // CHECK: [[GEP:%.*]] = index_raw_pointer
  // CHECK: return [[GEP]]
  return Builtin.gepRaw_Int32(p, i)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins3gep{{[_0-9a-zA-Z]*}}F
func gep<Elem>(_ p: Builtin.RawPointer, i: Builtin.Word, e: Elem.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[GEP:%.*]] = index_addr [stack_protection] [[P2A]] : $*Elem, %1 : $Builtin.Word
  // CHECK: [[A2P:%.*]] = address_to_pointer [stack_protection] [[GEP]]
  // CHECK: return [[A2P]]
  return Builtin.gep_Word(p, i, e)
}

public final class Header { }

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20allocWithTailElems_1{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_1<T>(n: Builtin.Word, ty: T.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T * %0 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_1(Header.self, n, ty)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20allocWithTailElems_3{{[_0-9a-zA-Z]*}}F
func allocWithTailElems_3<T1, T2, T3>(n1: Builtin.Word, ty1: T1.Type, n2: Builtin.Word, ty2: T2.Type, n3: Builtin.Word, ty3: T3.Type) -> Header {
  // CHECK: [[M:%.*]] = metatype $@thick Header.Type
  // CHECK: [[A:%.*]] = alloc_ref [tail_elems $T1 * %0 : $Builtin.Word] [tail_elems $T2 * %2 : $Builtin.Word] [tail_elems $T3 * %4 : $Builtin.Word] $Header
  // CHECK: return [[A]]
  return Builtin.allocWithTailElems_3(Header.self, n1, ty1, n2, ty2, n3, ty3)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins16projectTailElems{{[_0-9a-zA-Z]*}}F
func projectTailElems<T>(h: Header, ty: T.Type) -> Builtin.RawPointer {
  // CHECK: bb0([[ARG1:%.*]] : @guaranteed $Header
  // CHECK:   [[TA:%.*]] = ref_tail_addr [[ARG1]] : $Header
  // CHECK:   [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK:   return [[A2P]]
  return Builtin.projectTailElems(h, ty)
}
// CHECK: } // end sil function '$s8builtins16projectTailElems1h2tyBpAA6HeaderC_xmtlF'

// Make sure we borrow if this is owned.
//
// CHECK-LABEL: sil hidden [ossa] @$s8builtins21projectTailElemsOwned{{[_0-9a-zA-Z]*}}F
func projectTailElemsOwned<T>(h: __owned Header, ty: T.Type) -> Builtin.RawPointer {
  // CHECK: bb0([[ARG1:%.*]] : @owned $Header
  // CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
  // CHECK:   [[TA:%.*]] = ref_tail_addr [[BORROWED_ARG1]] : $Header
  //   -- Once we have passed the address through a2p, we no longer provide any guarantees.
  //   -- We still need to make sure that the a2p itself is in the borrow site though.
  // CHECK:   [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK:   end_borrow [[BORROWED_ARG1]]
  // CHECK:   destroy_value [[ARG1]]
  // CHECK:   return [[A2P]]
  return Builtin.projectTailElems(h, ty)
}
// CHECK: } // end sil function '$s8builtins21projectTailElemsOwned{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11getTailAddr{{[_0-9a-zA-Z]*}}F
func getTailAddr<T1, T2>(start: Builtin.RawPointer, i: Builtin.Word, ty1: T1.Type, ty2: T2.Type) -> Builtin.RawPointer {
  // CHECK: [[P2A:%.*]] = pointer_to_address %0
  // CHECK: [[TA:%.*]] = tail_addr [[P2A]] : $*T1, %1 : $Builtin.Word, $T2
  // CHECK: [[A2P:%.*]] = address_to_pointer [[TA]]
  // CHECK: return [[A2P]]
  return Builtin.getTailAddr_Word(start, i, ty1, ty2)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18protectedAddressOfyBpxzlF
func protectedAddressOf<T>(_ x: inout T) -> Builtin.RawPointer {
  // CHECK: [[A:%.*]] = begin_access [modify] [unknown] %0
  // CHECK: [[P:%.*]] = address_to_pointer [stack_protection] [[A]]
  // CHECK: return [[P]]
  return Builtin.addressof(&x)
}
// CHECK: } // end sil function '$s8builtins18protectedAddressOfyBpxzlF'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20unprotectedAddressOfyBpxzlF
func unprotectedAddressOf<T>(_ x: inout T) -> Builtin.RawPointer {
  // CHECK: [[A:%.*]] = begin_access [modify] [unknown] %0
  // CHECK: [[P:%.*]] = address_to_pointer [[A]]
  // CHECK: return [[P]]
  return Builtin.unprotectedAddressOf(&x)
}
// CHECK: } // end sil function '$s8builtins20unprotectedAddressOfyBpxzlF'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins24protectedAddressOfBorrowyBpxlF
func protectedAddressOfBorrow<T>(_ x: T) -> Builtin.RawPointer {
  // CHECK: [[P:%.*]] = address_to_pointer [stack_protection] %0
  // CHECK: return [[P]]
  return Builtin.addressOfBorrow(x)
}
// CHECK: } // end sil function '$s8builtins24protectedAddressOfBorrowyBpxlF'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins26unprotectedAddressOfBorrowyBpxlF
func unprotectedAddressOfBorrow<T>(_ x: T) -> Builtin.RawPointer {
  // CHECK: [[P:%.*]] = address_to_pointer %0
  // CHECK: return [[P]]
  return Builtin.unprotectedAddressOfBorrow(x)
}
// CHECK: } // end sil function '$s8builtins26unprotectedAddressOfBorrowyBpxlF'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins25beginUnpairedModifyAccess{{[_0-9a-zA-Z]*}}F
func beginUnpairedModifyAccess<T1>(address: Builtin.RawPointer, scratch: Builtin.RawPointer, ty1: T1.Type) {
  // CHECK: [[P2A_ADDR:%.*]] = pointer_to_address %0
  // CHECK: [[P2A_SCRATCH:%.*]] = pointer_to_address %1
  // CHECK: begin_unpaired_access [modify] [dynamic] [builtin] [[P2A_ADDR]] : $*T1, [[P2A_SCRATCH]] : $*Builtin.UnsafeValueBuffer
  // CHECK: [[RESULT:%.*]] = tuple ()
  // CHECK: [[RETURN:%.*]] = tuple ()
  // CHECK: return [[RETURN]] : $()

  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins30performInstantaneousReadAccess{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins15legacy_condfail{{[_0-9a-zA-Z]*}}F
func legacy_condfail(_ i: Builtin.Int1) {
  Builtin.condfail(i)
  // CHECK: cond_fail {{%.*}} : $Builtin.Int1, "unknown runtime failure"
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins8condfail{{[_0-9a-zA-Z]*}}F
func condfail(_ i: Builtin.Int1) {
  Builtin.condfail_message(i, StaticString("message").unsafeRawPointer)
  // CHECK: builtin "condfail_message"({{%.*}} : $Builtin.Int1, {{%.*}} : $Builtin.RawPointer) : $()
}

struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10canBeClass{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18canBeClassMetatype{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11fixLifetimeyyAA1CCF : $@convention(thin) (@guaranteed C) -> () {
func fixLifetime(_ c: C) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $C):
  // CHECK:   fix_lifetime [[ARG]] : $C
  Builtin.fixLifetime(c)
}
// CHECK: } // end sil function '$s8builtins11fixLifetimeyyAA1CCF'

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20assert_configuration{{[_0-9a-zA-Z]*}}F
func assert_configuration() -> Builtin.Int32 {
  return Builtin.assert_configuration()
  // CHECK: [[APPLY:%.*]] = builtin "assert_configuration"() : $Builtin.Int32
  // CHECK: return [[APPLY]] : $Builtin.Int32
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins17assumeNonNegativeyBwBwF
func assumeNonNegative(_ x: Builtin.Word) -> Builtin.Word {
  return Builtin.assumeNonNegative_Word(x)
  // CHECK: [[APPLY:%.*]] = builtin "assumeNonNegative_Word"(%0 : $Builtin.Word) : $Builtin.Word
  // CHECK: return [[APPLY]] : $Builtin.Word
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11autoreleaseyyAA1OCF : $@convention(thin) (@guaranteed O) -> () {
// ==> SEMANTIC ARC TODO: This will be unbalanced... should we allow it?
// CHECK: bb0([[ARG:%.*]] : @guaranteed $O):
// CHECK:   unmanaged_autorelease_value [[ARG]]
// CHECK: } // end sil function '$s8builtins11autoreleaseyyAA1OCF'
func autorelease(_ o: O) {
  Builtin.autorelease(o)
}

// The 'unreachable' builtin is emitted verbatim by SILGen and eliminated during
// diagnostics.

// CHECK-LABEL: sil hidden [ossa] @$s8builtins11unreachable{{[_0-9a-zA-Z]*}}F
// CHECK:         builtin "unreachable"()
// CHECK:         return
// CANONICAL-LABEL: sil hidden @$s8builtins11unreachableyyF : $@convention(thin) () -> () {
func unreachable() {
  Builtin.unreachable()
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins15reinterpretCast_1xBw_AA1DCAA1CCSgAGtAG_BwtF : $@convention(thin) (@guaranteed C, Builtin.Word) -> (Builtin.Word, @owned D, @owned Optional<C>, @owned C)
// CHECK:       bb0([[ARG1:%.*]] : @guaranteed $C, [[ARG2:%.*]] : $Builtin.Word):
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

// CHECK-LABEL: sil hidden [ossa] @$s8builtins19reinterpretAddrOnly{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnly<T, U>(_ t: T) -> U {
  // CHECK: unchecked_addr_cast {{%.*}} : $*T to $*U
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins28reinterpretAddrOnlyToTrivial{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnlyToTrivial<T>(_ t: T) -> Int {
  // CHECK: copy_addr %0 to [init] [[INPUT:%.*]] : $*T
  // CHECK: [[ADDR:%.*]] = unchecked_addr_cast [[INPUT]] : $*T to $*Int
  // CHECK: [[VALUE:%.*]] = load [trivial] [[ADDR]]
  // CHECK: destroy_addr [[INPUT]]
  return Builtin.reinterpretCast(t)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins27reinterpretAddrOnlyLoadable{{[_0-9a-zA-Z]*}}F
func reinterpretAddrOnlyLoadable<T>(_ a: Int, _ b: T) -> (T, Int) {
  // CHECK: [[BUF:%.*]] = alloc_stack $Int
  // CHECK: store {{%.*}} to [trivial] [[BUF]]
  // CHECK: [[RES1:%.*]] = unchecked_addr_cast [[BUF]] : $*Int to $*T
  // CHECK: copy_addr [[RES1]] to [init]
  return (Builtin.reinterpretCast(a) as T,
  // CHECK: [[RES:%.*]] = unchecked_addr_cast {{%.*}} : $*T to $*Int
  // CHECK: load [trivial] [[RES]]
          Builtin.reinterpretCast(b) as Int)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18castToBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         [[ARG_COPY:%.*]] = copy_value %0
// CHECK:         [[BO:%.*]] = ref_to_bridge_object [[ARG_COPY]] : $C, {{%.*}} : $Builtin.Word
// CHECK-NOT:     destroy_value [[ARG_COPY]]
// CHECK:         return [[BO]]
func castToBridgeObject(_ c: C, _ w: Builtin.Word) -> Builtin.BridgeObject {
  return Builtin.castToBridgeObject(c, w)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins23castRefFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_ref [[BO:%.*]] : $Builtin.BridgeObject to $C
func castRefFromBridgeObject(_ bo: Builtin.BridgeObject) -> C {
  return Builtin.castReferenceFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins30castBitPatternFromBridgeObject{{[_0-9a-zA-Z]*}}F
// CHECK:         bridge_object_to_word [[BO:%.*]] : $Builtin.BridgeObject to $Builtin.Word
// CHECK-NOT:         destroy_value [[BO]]
func castBitPatternFromBridgeObject(_ bo: Builtin.BridgeObject) -> Builtin.Word {
  return Builtin.castBitPatternFromBridgeObject(bo)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins16beginCOWMutationySbAA1CCzF
// CHECK:     [[L:%.*]] = load [take] [[ADDR:%[0-9]*]]
// CHECK:     ([[U:%.*]], [[B:%.*]]) = begin_cow_mutation [[L]]
// CHECK:     store [[B]] to [init] [[ADDR]]
// CHECK:     apply {{%[0-9]*}}([[U]]
func beginCOWMutation(_ c: inout C) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.beginCOWMutation(&c))
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins23beginCOWMutation_nativeySbAA1CCzF
// CHECK:     [[L:%.*]] = load [take] [[ADDR:%[0-9]*]]
// CHECK:     ([[U:%.*]], [[B:%.*]]) = begin_cow_mutation [native] [[L]]
// CHECK:     store [[B]] to [init] [[ADDR]]
// CHECK:     apply {{%[0-9]*}}([[U]]
func beginCOWMutation_native(_ c: inout C) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.beginCOWMutation_native(&c))
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins14endCOWMutationyyAA1CCzF
// CHECK:     [[L:%.*]] = load [take] [[ADDR:%[0-9]*]]
// CHECK:     [[B:%.*]] = end_cow_mutation [[L]]
// CHECK:     store [[B]] to [init] [[ADDR]]
func endCOWMutation(_ c: inout C) {
  Builtin.endCOWMutation(&c)
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// NativeObject
// CHECK-LABEL: sil hidden [ossa] @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Optional<Builtin.NativeObject>):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Builtin.NativeObject>
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Optional<Builtin.NativeObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject?) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique(&ref))
}

// NativeObject nonNull
// CHECK-LABEL: sil hidden [ossa] @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.NativeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Builtin.NativeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.NativeObject) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique(&ref))
}

// AnyObject (ObjC)
// CHECK-LABEL: sil hidden [ossa] @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Optional<AnyObject>):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Optional<AnyObject>
// CHECK: return
func isUnique(_ ref: inout Builtin.AnyObject?) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique(&ref))
}

// AnyObject (ObjC) nonNull
// CHECK-LABEL: sil hidden [ossa] @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*AnyObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*AnyObject
// CHECK: return
func isUnique(_ ref: inout Builtin.AnyObject) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique(&ref))
}

// BridgeObject nonNull
// CHECK-LABEL: sil hidden [ossa] @$s8builtins8isUnique{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[BUILTIN:%.*]] = is_unique [[WRITE]] : $*Builtin.BridgeObject
// CHECK: return
func isUnique(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique(&ref))
}

// BridgeObject nonNull native
// CHECK-LABEL: sil hidden [ossa] @$s8builtins15isUnique_native{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Builtin.BridgeObject):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK: [[CAST:%.*]] = unchecked_addr_cast [[WRITE]] : $*Builtin.BridgeObject to $*Builtin.NativeObject
// CHECK: return
func isUnique_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isUnique_native(&ref))
}

// ----------------------------------------------------------------------------
// Builtin.castReference
// ----------------------------------------------------------------------------

class A {}
protocol PUnknown {}
protocol PClass : class {}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins19refcast_generic_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr  T in %{{.*}} : $*T to AnyObject in %{{.*}} : $*AnyObject
func refcast_generic_any<T>(_ o: T) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins17refcast_class_anyyyXlAA1ACF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $A):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[ARG_CASTED:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $A to $AnyObject
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[ARG_CASTED]]
// CHECK: } // end sil function '$s8builtins17refcast_class_anyyyXlAA1ACF'
func refcast_class_any(_ o: A) -> AnyObject {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20refcast_punknown_any{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr any PUnknown in %{{.*}} : $*any PUnknown to AnyObject in %{{.*}} : $*AnyObject
func refcast_punknown_any(_ o: PUnknown) -> AnyObject {
  return Builtin.castReference(o as PUnknown)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins18refcast_pclass_anyyyXlAA6PClass_pF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any PClass):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[ARG_CAST:%.*]] = unchecked_ref_cast [[ARG_COPY]] : $any PClass to $AnyObject
// CHECK:   return [[ARG_CAST]]
// CHECK: } // end sil function '$s8builtins18refcast_pclass_anyyyXlAA6PClass_pF'
func refcast_pclass_any(_ o: PClass) -> AnyObject {
  return Builtin.castReference(o as PClass)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins20refcast_any_punknown{{[_0-9a-zA-Z]*}}F
// CHECK: unchecked_ref_cast_addr AnyObject in %{{.*}} : $*AnyObject to any PUnknown in %{{.*}} : $*any PUnknown
func refcast_any_punknown(_ o: AnyObject) -> PUnknown {
  return Builtin.castReference(o)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10bindMemory{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $Builtin.RawPointer, [[I:%.*]] : $Builtin.Word, [[T:%.*]] : $@thick T.Type):
// CHECK: %{{.*}} = bind_memory [[P]] : $Builtin.RawPointer, [[I]] : $Builtin.Word to $*T
// CHECK:   return {{%.*}} : $()
// CHECK: }
func bindMemory<T>(ptr: Builtin.RawPointer, idx: Builtin.Word, _: T.Type) {
  Builtin.bindMemory(ptr, idx, T.self)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins12rebindMemory{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%.*]] : $Builtin.RawPointer, [[I:%.*]] : $Builtin.Word, [[T:%.*]] : $@thick T.Type):
// CHECK: [[BIND:%.*]] = bind_memory [[P]] : $Builtin.RawPointer, [[I]] : $Builtin.Word to $*T
// CHECK: [[MV1:%.*]] = move_value [var_decl] [[BIND]] : $Builtin.Word
// CHECK: [[REBIND:%.*]] = rebind_memory [[P]] : $Builtin.RawPointer to [[MV1]] : $Builtin.Word
// CHECK: [[MV2:%.*]] = move_value [var_decl] [[REBIND]] : $Builtin.Word
// CHECK: %{{.*}} = rebind_memory [[P]] : $Builtin.RawPointer to [[MV2]] : $Builtin.Word
// CHECK:   return {{%.*}} : $()
// CHECK: }
func rebindMemory<T>(ptr: Builtin.RawPointer, idx: Builtin.Word, _: T.Type) {
  let previousBindings = Builtin.bindMemory(ptr, idx, T.self)
  let genericBinding = Builtin.rebindMemory(ptr, previousBindings)
  Builtin.rebindMemory(ptr, genericBinding)
}

//===----------------------------------------------------------------------===//
// RC Operations
//===----------------------------------------------------------------------===//

// SILGen test:
//
// CHECK-LABEL: sil hidden [ossa] @$s8builtins6retain{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
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
// CHECK-LABEL: sil hidden [ossa] @$s8builtins7release{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Builtin.NativeObject) -> () {
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
// CANONICAL-NEXT:   return
// CANONICAL: } // end sil function '$s8builtins7release{{[_0-9a-zA-Z]*}}F'

func release(ptr: Builtin.NativeObject) {
  Builtin.release(ptr)
}

//===----------------------------------------------------------------------===//
// Other Operations
//===----------------------------------------------------------------------===//

func once_helper(_ context: Builtin.RawPointer) {}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins4once7controlyBp_tF
// CHECK:      [[T0:%.*]] = function_ref @$s8builtins11once_helperyyBpFTo : $@convention(c) (Builtin.RawPointer) -> ()
// CHECK-NEXT: builtin "once"(%0 : $Builtin.RawPointer, [[T0]] : $@convention(c) (Builtin.RawPointer) -> ())
func once(control: Builtin.RawPointer) {
  Builtin.once(control, once_helper)
}


// CHECK-LABEL: sil hidden [ossa] @$s8builtins19valueToBridgeObjectyBbSuF : $@convention(thin) (UInt) -> @owned Builtin.BridgeObject {
// CHECK: bb0([[UINT:%.*]] : $UInt):
// CHECK:   [[BI:%.*]] = struct_extract [[UINT]] : $UInt, #UInt._value
// CHECK:   [[CAST:%.*]] = value_to_bridge_object [[BI]]
// CHECK:   [[RET:%.*]] = copy_value [[CAST]] : $Builtin.BridgeObject
// CHECK:   return [[RET]] : $Builtin.BridgeObject
// CHECK: } // end sil function '$s8builtins19valueToBridgeObjectyBbSuF'
func valueToBridgeObject(_ x: UInt) -> Builtin.BridgeObject {
  return Builtin.valueToBridgeObject(x._value)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10assumeTrueyyBi1_F
// CHECK: builtin "assume_Int1"({{.*}} : $Builtin.Int1)
// CHECK: return
func assumeTrue(_ x: Builtin.Int1) {
  Builtin.assume_Int1(x)
}

// CHECK: sil hidden [ossa] @$s8builtins15assumeAlignmentyyBp_BwtF : $@convention(thin) (Builtin.RawPointer, Builtin.Word) -> () {  
// CHECK: builtin "assumeAlignment"(%{{.*}} : $Builtin.RawPointer, %{{.*}} : $Builtin.Word) : $Builtin.RawPointer
// CHECK: return
func assumeAlignment(_ p: Builtin.RawPointer, _ x: Builtin.Word) {
  Builtin.assumeAlignment(p, x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins9packCountyBwxxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> Builtin.Word {
// CHECK: bb0(%0 : $*Pack{repeat each T}):
// CHECK:   [[META:%.*]] = metatype $@thin (repeat each T).Type
// CHECK:   [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each T}
// CHECK:   return [[PACK_LENGTH]] : $Builtin.Word
func packCount<each T>(_ x: repeat each T) -> Builtin.Word {
  Builtin.packLength((repeat each T).self)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins10getEnumTagyBi32_xlF : $@convention(thin) <T> (@in_guaranteed T) -> Builtin.Int32 {
// CHECK: bb0([[INPUT:%.*]] : $*T):
// CHECK-NOT: copy_addr
// CHECK:   [[TAG:%.*]] = builtin "getEnumTag"<T>([[INPUT]] : $*T)
// CHECK:   return [[TAG]] : $Builtin.Int32
func getEnumTag<T>(_ x: T) -> Builtin.Int32 {
  Builtin.getEnumTag(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s8builtins13injectEnumTag_3tagyxz_Bi32_tlF : $@convention(thin) <T> (@inout T, Builtin.Int32) -> () {
// CHECK: bb0([[INPUT:%.*]] : $*T, [[TAG:%.*]] : $Builtin.Int32):
// CHECK-NOT: copy_addr
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[INPUT]] : $*T
// CHECK:   builtin "injectEnumTag"<T>([[ACCESS]] : $*T, [[TAG]] : $Builtin.Int32)
// CHECK:   end_access [[ACCESS]]
func injectEnumTag<T>(_ x: inout T, tag: Builtin.Int32) {
  Builtin.injectEnumTag(&x, tag)
}

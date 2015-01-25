// RUN: %target-swift-frontend -parse-stdlib -emit-sil %s | FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s -check-prefix=SILGEN

import Swift

struct A {
  var base: UnsafeMutablePointer<Int32> = nil

  subscript(index: Int32) -> Int32 {
    unsafeAddress {
      return UnsafePointer(base)
    }
    unsafeMutableAddress {
      return base
    }
  }
}

// CHECK-LABEL: sil hidden @_TFV10addressors1Alu9subscriptFVSs5Int32S1_ : $@cc(method) @thin (Int32, A) -> UnsafePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $A):
// CHECK:   [[BASE:%.*]] = struct_extract [[SELF]] : $A, #A.base
// CHECK:   [[T0:%.*]] = struct_extract [[BASE]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T1:%.*]] = struct $UnsafePointer<Int32> ([[T0]] : $Builtin.RawPointer)
// CHECK:   return [[T1]] : $UnsafePointer<Int32>

// CHECK-LABEL: sil hidden @_TFV10addressors1Aau9subscriptFVSs5Int32S1_ : $@cc(method) @thin (Int32, @inout A) -> UnsafeMutablePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*A):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*A, #A.base
// CHECK:   [[BASE:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   return [[BASE]] : $UnsafeMutablePointer<Int32>

// CHECK-LABEL: sil hidden @_TF10addressors5test0FT_T_ : $@thin () -> () {
func test0() {
// CHECK: [[A:%.*]] = alloc_stack $A
// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1ACfMS0_FT_S0_
// CHECK: [[T1:%.*]] = metatype $@thin A.Type
// CHECK: [[AVAL:%.*]] = apply [[T0]]([[T1]]) 
// CHECK: store [[AVAL]] to [[A]]#1
  var a = A()

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Alu9subscriptFVSs5Int32S1_ :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[AVAL]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK: [[Z:%.*]] = load [[T3]] : $*Int32
  let z = a[10]

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aau9subscriptFVSs5Int32S1_ :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]]#1)
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK: load
// CHECK: sadd_with_overflow_Int{{32|64}}
// CHECK: store {{%.*}} to [[T3]]
  a[5] += z

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aau9subscriptFVSs5Int32S1_ :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]]#1)
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK: store {{%.*}} to [[T3]]
  a[3] = 6
}

// CHECK-LABEL: sil hidden @_TF10addressors5test1FT_VSs5Int32 : $@thin () -> Int32
func test1() -> Int32 {
// CHECK: [[CTOR:%.*]] = function_ref @_TFV10addressors1ACfMS0_FT_S0_
// CHECK: [[T0:%.*]] = metatype $@thin A.Type
// CHECK: [[A:%.*]] = apply [[CTOR]]([[T0]]) : $@thin (@thin A.Type) -> A
// CHECK: [[ACCESSOR:%.*]] = function_ref @_TFV10addressors1Alu9subscriptFVSs5Int32S1_ : $@cc(method) @thin (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[PTR:%.*]] = apply [[ACCESSOR]]({{%.*}}, [[A]]) : $@cc(method) @thin (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[T0:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK: [[T2:%.*]] = load [[T1]] : $*Int32
// CHECK: return [[T2]] : $Int32
  return A()[0]
}

let uninitAddr = UnsafeMutablePointer<Int32>.alloc(1)
var global: Int32 {
  unsafeAddress {
    return UnsafePointer(uninitAddr)
  }
// CHECK: sil hidden @_TF10addressorslu6globalVSs5Int32 : $@thin () -> UnsafePointer<Int32> {
// CHECK:   [[T0:%.*]] = global_addr @_Tv10addressors10uninitAddrGVSs20UnsafeMutablePointerVSs5Int32_ : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T1:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = struct $UnsafePointer<Int32> ([[T2]] : $Builtin.RawPointer)
// CHECK:   return [[T3]] : $UnsafePointer<Int32>
}

func test_global() -> Int32 {
  return global
}
// CHECK: sil hidden @_TF10addressors11test_globalFT_VSs5Int32 : $@thin () -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_TF10addressorslu6globalVSs5Int32 : $@thin () -> UnsafePointer<Int32>
// CHECK:   [[T1:%.*]] = apply [[T0]]() : $@thin () -> UnsafePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T4:%.*]] = load [[T3]] : $*Int32
// CHECK:   return [[T4]] : $Int32

// Test that having generated trivial accessors for something because
// of protocol conformance doesn't force us down inefficient access paths.
protocol Subscriptable {
  subscript(i: Int32) -> Int32 { get set }
}

struct B : Subscriptable {
  subscript(i: Int32) -> Int32 {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}

// CHECK: sil hidden @_TF10addressors6test_BFRVS_1BT_ : $@thin (@inout B) -> () {
// CHECK: bb0([[B:%.*]] : $*B):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 0
// CHECK:   [[INDEX:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   [[RHS:%.*]] = integer_literal $Builtin.Int32, 7
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Bau9subscriptFVSs5Int32S1_
// CHECK:   [[PTR:%.*]] = apply [[T0]]([[INDEX]], [[B]])
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// Accept either of struct_extract+load or load+struct_element_addr.
// CHECK:   load
// CHECK:   [[T1:%.*]] = builtin "or_Int32"
// CHECK:   [[T2:%.*]] = struct $Int32 ([[T1]] : $Builtin.Int32)
// CHECK:   store [[T2]] to [[ADDR]] : $*Int32
func test_B(inout b: B) {
  b[0] |= 7
}

// Test that we handle abstraction difference.
struct CArray<T> {
  var storage: UnsafeMutablePointer<T> = nil
  subscript(index: Int) -> T {
    unsafeAddress { return UnsafePointer(storage) + index }
    unsafeMutableAddress { return storage + index }
  }
}

func id_int(i: Int32) -> Int32 { return i }

// CHECK-LABEL: sil hidden @_TF10addressors11test_carrayFRGVS_6CArrayFVSs5Int32S1__S1_ : $@thin (@inout CArray<Int32 -> Int32>) -> Int32 {
// CHECK: bb0([[ARRAY:%.*]] : $*CArray<Int32 -> Int32>):
func test_carray(inout array: CArray<Int32 -> Int32>) -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors6CArrayau9subscriptFSiQ_ :
// CHECK:   [[T1:%.*]] = apply [[T0]]<Int32 -> Int32>({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32 -> Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*@callee_owned (@out Int32, @in Int32) -> ()
// CHECK:   store {{%.*}} to [[T3]] :
  array[0] = id_int

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_TFV10addressors6CArraylu9subscriptFSiQ_ :
// CHECK:   [[T2:%.*]] = apply [[T1]]<Int32 -> Int32>({{%.*}}, [[T0]])
// CHECK:   [[T3:%.*]] = struct_extract [[T2]] : $UnsafePointer<Int32 -> Int32>, #UnsafePointer._rawValue
// CHECK:   [[T4:%.*]] = pointer_to_address [[T3]] : $Builtin.RawPointer to $*@callee_owned (@out Int32, @in Int32) -> ()
// CHECK:   [[T5:%.*]] = load [[T4]]
  return array[1](5)
}

// rdar://17270560, redux
struct D : Subscriptable {
  subscript(i: Int32) -> Int32 {
    get { return i }
    unsafeMutableAddress { return nil }
  }
}
// Setter.
// SILGEN-LABEL: sil hidden [transparent] @_TFV10addressors1Ds9subscriptFVSs5Int32S1_
// SILGEN: bb0([[VALUE:%.*]] : $Int32, [[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   debug_value [[VALUE]] : $Int32
// SILGEN:   debug_value [[I]] : $Int32
// SILGEN:   [[BOX:%.*]] = alloc_box $D
// SILGEN:   copy_addr [[SELF]] to [initialization] [[BOX]]#1 : $*D     // id: %6
// SILGEN:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFVSs5Int32S1_{{.*}}
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[BOX]]#1)
// SILGEN:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// SILGEN:   assign [[VALUE]] to [[ADDR]] : $*Int32

// materializeForSet.
// SILGEN: sil hidden [transparent] @_TFV10addressors1Dm9subscriptFVSs5Int32S1_
// SILGEN: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   debug_value [[BUFFER]]
// SILGEN:   debug_value [[I]]
// SILGEN:   [[BOX:%.*]] = alloc_box $D
// SILGEN:   copy_addr [[SELF]] to [initialization] [[BOX]]#1 : $*D
// SILGEN:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFVSs5Int32S1_
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[BOX]]#1)
// SILGEN:   [[ADDR:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[INIT:%.*]] = function_ref @_TFSqCU__fMGSqQ__FT10nilLiteralT__GSqQ__
// SILGEN:   [[META:%.*]] = metatype $@thin Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>.Type
// SILGEN:   [[T0:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>
// SILGEN:   [[OPT:%.*]] = apply [transparent] [[INIT]]<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>([[T0]]#1, [[META]])
// SILGEN:   [[T1:%.*]] = load [[T0]]#1
// SILGEN:   [[T2:%.*]] = tuple ([[ADDR]] : $Builtin.RawPointer, [[T1]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>)
// SILGEN:   dealloc_stack [[T0]]#0
// SILGEN:   copy_addr [[BOX]]#1 to [[SELF]]
// SILGEN:   strong_release [[BOX]]#0
// SILGEN:   return [[T2]] :

func make_int() -> Int32 { return 0 }
func take_int_inout(inout value: Int32) {}

// CHECK-LABEL: sil hidden @_TF10addressors6test_dFRVS_1DVSs5Int32 : $@thin (@inout D) -> Int32
// CHECK: bb0([[ARRAY:%.*]] : $*D):
func test_d(inout array: D) -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_TF10addressors8make_intFT_VSs5Int32
// CHECK:   [[V:%.*]] = apply [[T0]]()
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFVSs5Int32S1_
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK:   store [[V]] to [[ADDR]] : $*Int32
  array[0] = make_int()

// CHECK:   [[FN:%.*]] = function_ref @_TF10addressors14take_int_inoutFRVSs5Int32T_
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFVSs5Int32S1_
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int32
// CHECK:   apply [[FN]]([[ADDR]])
  take_int_inout(&array[1])

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_TFV10addressors1Dg9subscriptFVSs5Int32S1_ 
// CHECK:   [[T2:%.*]] = apply [[T1]]({{%.*}}, [[T0]])
// CHECK:   return [[T2]]
  return array[2]
}

struct E {
  var value: Int32 {
    unsafeAddress { return nil }
    nonmutating unsafeMutableAddress { return nil }
  }
}

// CHECK-LABEL: sil hidden @_TF10addressors6test_eFVS_1ET_
// CHECK: bb0([[E:%.*]] : $E):
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Eau5valueVSs5Int32
// CHECK:   [[T1:%.*]] = apply [[T0]]([[E]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]]
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]]
// CHECK:   store {{%.*}} to [[T3]] : $*Int32
func test_e(e: E) {
  e.value = 0
}

class F {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.alloc(100)

  final var value: Int32 {
    addressWithNativeOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithNativeOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}

// CHECK: sil hidden @_TFC10addressors1Flo5valueVSs5Int32 : $@cc(method) @thin (@owned F) -> @owned (UnsafePointer<Int32>, Builtin.NativeObject) {
// CHECK: sil hidden @_TFC10addressors1Fao5valueVSs5Int32 : $@cc(method) @thin (@owned F) -> @owned (UnsafeMutablePointer<Int32>, Builtin.NativeObject) {

func test_f0(f: F) -> Int32 {
  return f.value
}
// CHECK: sil hidden @_TF10addressors7test_f0FCS_1FVSs5Int32 : $@thin (@owned F) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   strong_retain [[SELF]] : $F
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Flo5valueVSs5Int32 : $@cc(method) @thin (@owned F) -> @owned (UnsafePointer<Int32>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F
// CHECK:   return [[VALUE]] : $Int32

func test_f1(f: F) {
  f.value = 14
}
// CHECK: sil hidden @_TF10addressors7test_f1FCS_1FT_ : $@thin (@owned F) -> () {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 14
// CHECK:   [[VALUE:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   strong_retain [[SELF]] : $F
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Fao5valueVSs5Int32 : $@cc(method) @thin (@owned F) -> @owned (UnsafeMutablePointer<Int32>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F

class G {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.alloc(100)

  var value: Int32 {
    addressWithNativeOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithNativeOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}
// CHECK: sil hidden [transparent] @_TFC10addressors1Gg5valueVSs5Int32 : $@cc(method) @thin (@owned G) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $G):
// CHECK:   strong_retain [[SELF]]
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Glo5valueVSs5Int32 : $@cc(method) @thin (@owned G) -> @owned (UnsafePointer<Int32>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $G
// CHECK:   return [[VALUE]] : $Int32

// CHECK: sil hidden [transparent] @_TFC10addressors1Gs5valueVSs5Int32 : $@cc(method) @thin (Int32, @owned G) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int32, [[SELF:%1]] : $G):
// CHECK:   strong_retain [[SELF]] : $G
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Gao5valueVSs5Int32 : $@cc(method) @thin (@owned G) -> @owned (UnsafeMutablePointer<Int32>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $G

//   materializeForSet for G.value
// CHECK: sil hidden [transparent] @_TFC10addressors1Gm5valueVSs5Int32 : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @owned G) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $G):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Gao5valueVSs5Int32 : $@cc(method) @thin (@owned G) -> @owned (UnsafeMutablePointer<Int32>, Builtin.NativeObject)
// CHECK:   strong_retain [[SELF]] : $G
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[TUPLE:%.*]] = tuple ([[T1]] : $UnsafeMutablePointer<Int32>, [[T2]] : $Builtin.NativeObject)
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   retain_value [[TUPLE]]
// CHECK:   [[T3:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   store [[T3]] to [[T2]] : $*Builtin.NativeObject
//   Pull out the address.
// CHECK:   [[T0:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[PTR:%.*]] = struct_extract [[T0]] :
//   Set up the callback.
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>
// CHECK:   [[T0:%.*]] = init_enum_data_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[T1:%.*]] = function_ref @_TFFC10addressors1Gm5valueVSs5Int32U_FTBpRBBRS0_MS0__T_ :
// CHECK:   store [[T1]] to [[T0]] :
// CHECK:   inject_enum_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[CALLBACK:%.*]] = load [[TEMP]]#1 :
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>)
// CHECK:   release_value [[TUPLE]]
// CHECK:   strong_release [[SELF]]
// CHECK:   return [[RESULT]]

//   materializeForSet callback for G.value
// CHECK: sil @_TFFC10addressors1Gm5valueVSs5Int32U_FTBpRBBRS0_MS0__T_ : $@thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout G, @thick G.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*G, [[SELFTYPE:%3]] : $@thick G.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   [[OWNER:%.*]] = load [[T2]]
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   dealloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer

class H {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.alloc(100)

  final var value: Int32 {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}

// CHECK: sil hidden @_TFC10addressors1Hlp5valueVSs5Int32 : $@cc(method) @thin (@owned H) -> @owned (UnsafePointer<Int32>, Optional<Builtin.NativeObject>) {
// CHECK: sil hidden @_TFC10addressors1Hap5valueVSs5Int32 : $@cc(method) @thin (@owned H) -> @owned (UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>) {

func test_h0(f: H) -> Int32 {
  return f.value
}
// CHECK-LABEL: sil hidden @_TF10addressors7test_h0FCS_1HVSs5Int32 : $@thin (@owned H) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   strong_retain [[SELF]] : $H
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Hlp5valueVSs5Int32 : $@cc(method) @thin (@owned H) -> @owned (UnsafePointer<Int32>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H
// CHECK:   return [[VALUE]] : $Int32

func test_h1(f: H) {
  f.value = 14
}
// CHECK: sil hidden @_TF10addressors7test_h1FCS_1HT_ : $@thin (@owned H) -> () {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 14
// CHECK:   [[VALUE:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   strong_retain [[SELF]] : $H
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Hap5valueVSs5Int32 : $@cc(method) @thin (@owned H) -> @owned (UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H

class I {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.alloc(100)

  var value: Int32 {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}
// CHECK-LABEL: sil hidden [transparent] @_TFC10addressors1Ig5valueVSs5Int32 : $@cc(method) @thin (@owned I) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $I):
// CHECK:   strong_retain [[SELF]]
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Ilp5valueVSs5Int32 : $@cc(method) @thin (@owned I) -> @owned (UnsafePointer<Int32>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $I
// CHECK:   return [[VALUE]] : $Int32

// CHECK-LABEL: sil hidden [transparent] @_TFC10addressors1Is5valueVSs5Int32 : $@cc(method) @thin (Int32, @owned I) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int32, [[SELF:%1]] : $I):
// CHECK:   strong_retain [[SELF]] : $I
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Iap5valueVSs5Int32 : $@cc(method) @thin (@owned I) -> @owned (UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $I

// CHECK-LABEL: sil hidden [transparent] @_TFC10addressors1Im5valueVSs5Int32 : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @owned I) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $I):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Iap5valueVSs5Int32 : $@cc(method) @thin (@owned I) -> @owned (UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>)
// CHECK:   strong_retain [[SELF]] : $I
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[TUPLE:%.*]] = tuple ([[T1]] : $UnsafeMutablePointer<Int32>, [[T2]] : $Optional<Builtin.NativeObject>)
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   retain_value [[TUPLE]]
// CHECK:   [[T3:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   store [[T3]] to [[T2]] : $*Optional<Builtin.NativeObject>
//   Pull out the address.
// CHECK:   [[T0:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[PTR:%.*]] = struct_extract [[T0]] :
//   Set up the callback.
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>
// CHECK:   [[T0:%.*]] = init_enum_data_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[T1:%.*]] = function_ref @_TFFC10addressors1Im5valueVSs5Int32U_FTBpRBBRS0_MS0__T_ :
// CHECK:   store [[T1]] to [[T0]] :
// CHECK:   inject_enum_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[CALLBACK:%.*]] = load [[TEMP]]#1 :
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>)
// CHECK:   release_value [[TUPLE]]
// CHECK:   strong_release [[SELF]]
// CHECK:   return [[RESULT]]

//   materializeForSet callback for I.value
// CHECK-LABEL: sil @_TFFC10addressors1Im5valueVSs5Int32U_FTBpRBBRS0_MS0__T_ : $@thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout I, @thick I.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*I, [[SELFTYPE:%3]] : $@thick I.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   [[OWNER:%.*]] = load [[T2]]
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   dealloc_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer

struct RecInner {
  subscript(i: Int32) -> Int32 {
    mutating get { return i }
  }
}
struct RecMiddle {
  var inner: RecInner
}
class RecOuter {
  var data: UnsafeMutablePointer<RecMiddle> = UnsafeMutablePointer.alloc(100)
  final var middle: RecMiddle {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}
func test_rec(outer: RecOuter) -> Int32 {
  return outer.middle.inner[0]
}
// This uses the mutable addressor.
// CHECK-LABEL: sil hidden @_TF10addressors8test_recFCS_8RecOuterVSs5Int32 : $@thin (@owned RecOuter) -> Int32 {
// CHECK:   function_ref @_TFC10addressors8RecOuterap6middleVS_9RecMiddle
// CHECK:   struct_element_addr {{.*}} : $*RecMiddle, #RecMiddle.inner
// CHECK:   function_ref @_TFV10addressors8RecInnerg9subscriptFVSs5Int32S1_ 

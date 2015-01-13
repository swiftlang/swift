// RUN: %swift -parse-stdlib -emit-sil %s | FileCheck %s

import Swift

struct A {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    unsafeAddress {
      return UnsafePointer(base)
    }
    unsafeMutableAddress {
      return base
    }
  }
}

// CHECK-LABEL: sil hidden @_TFV10addressors1Alu9subscriptFSiSi : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: bb0([[INDEX:%.*]] : $Int, [[SELF:%.*]] : $A):
// CHECK:   [[BASE:%.*]] = struct_extract [[SELF]] : $A, #A.base
// CHECK:   [[T0:%.*]] = struct_extract [[BASE]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T1:%.*]] = struct $UnsafePointer<Int> ([[T0]] : $Builtin.RawPointer)
// CHECK:   return [[T1]] : $UnsafePointer<Int>

// CHECK-LABEL: sil hidden @_TFV10addressors1Aau9subscriptFSiSi : $@cc(method) @thin (Int, @inout A) -> UnsafeMutablePointer<Int>
// CHECK: bb0([[INDEX:%.*]] : $Int, [[SELF:%.*]] : $*A):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*A, #A.base
// CHECK:   [[BASE:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int>
// CHECK:   return [[BASE]] : $UnsafeMutablePointer<Int>

// CHECK-LABEL: sil hidden @_TF10addressors5test0FT_T_ : $@thin () -> () {
func test0() {
// CHECK: [[A:%.*]] = alloc_stack $A
// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1ACfMS0_FT_S0_
// CHECK: [[T1:%.*]] = metatype $@thin A.Type
// CHECK: [[AVAL:%.*]] = apply [[T0]]([[T1]]) 
// CHECK: store [[AVAL]] to [[A]]#1
  var a = A()

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Alu9subscriptFSiSi :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[AVAL]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK: [[Z:%.*]] = load [[T3]] : $*Int
  let z = a[10]

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aau9subscriptFSiSi :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]]#1)
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK: load
// CHECK: sadd_with_overflow_Word
// CHECK: store {{%.*}} to [[T3]]
  a[5] += z

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aau9subscriptFSiSi :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]]#1)
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK: store {{%.*}} to [[T3]]
  a[3] = 6
}

// CHECK: sil hidden @_TF10addressors5test1FT_Si : $@thin () -> Int
func test1() -> Int {
// CHECK: [[CTOR:%.*]] = function_ref @_TFV10addressors1ACfMS0_FT_S0_
// CHECK: [[T0:%.*]] = metatype $@thin A.Type
// CHECK: [[A:%.*]] = apply [[CTOR]]([[T0]]) : $@thin (@thin A.Type) -> A
// CHECK: [[ACCESSOR:%.*]] = function_ref @_TFV10addressors1Alu9subscriptFSiSi : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: [[PTR:%.*]] = apply [[ACCESSOR]]({{%.*}}, [[A]]) : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: [[T0:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK: [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK: [[T2:%.*]] = load [[T1]] : $*Int
// CHECK: return [[T2]] : $Int
  return A()[0]
}

let uninitAddr = UnsafeMutablePointer<Int>.alloc(1)
var global: Int {
  unsafeAddress {
    return UnsafePointer(uninitAddr)
  }
// CHECK: sil hidden @_TF10addressorslu6globalSi : $@thin () -> UnsafePointer<Int> {
// CHECK:   [[T0:%.*]] = global_addr @_Tv10addressors10uninitAddrGVSs20UnsafeMutablePointerSi_ : $*UnsafeMutablePointer<Int>
// CHECK:   [[T1:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = struct $UnsafePointer<Int> ([[T2]] : $Builtin.RawPointer)
// CHECK:   return [[T3]] : $UnsafePointer<Int>
}

func test_global() -> Int {
  return global
}
// CHECK: sil hidden @_TF10addressors11test_globalFT_Si : $@thin () -> Int {
// CHECK:   [[T0:%.*]] = function_ref @_TF10addressorslu6globalSi : $@thin () -> UnsafePointer<Int>
// CHECK:   [[T1:%.*]] = apply [[T0]]() : $@thin () -> UnsafePointer<Int>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T4:%.*]] = load [[T3]] : $*Int
// CHECK:   return [[T4]] : $Int

// Test that having generated trivial accessors for something because
// of protocol conformance doesn't force us down inefficient access paths.
protocol Subscriptable {
  subscript(i: Int) -> Int { get set }
}

struct B : Subscriptable {
  subscript(i: Int) -> Int {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}

// CHECK: sil hidden @_TF10addressors6test_BFRVS_1BT_ : $@thin (@inout B) -> () {
// CHECK: bb0([[B:%.*]] : $*B):
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Bau9subscriptFSiSi
// CHECK:   [[PTR:%.*]] = apply [[T0]]({{.*}}, [[B]])
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   integer_literal $Builtin.Word, 7
// CHECK:   load
// CHECK:   [[T1:%.*]] = builtin "or_Word"
// CHECK:   [[T2:%.*]] = struct $Int ([[T1]] : $Builtin.Word)
// CHECK:   store [[T2]] to [[ADDR]] : $*Int
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

func id_int(i: Int) -> Int { return i }

// CHECK: sil hidden @_TF10addressors11test_carrayFRGVS_6CArrayFSiSi_Si : $@thin (@inout CArray<Int -> Int>) -> Int {
// CHECK: bb0([[ARRAY:%.*]] : $*CArray<Int -> Int>):
func test_carray(inout array: CArray<Int -> Int>) -> Int {
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors6CArrayau9subscriptFSiQ_ :
// CHECK:   [[T1:%.*]] = apply [[T0]]<Int -> Int>({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int -> Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*@callee_owned (@out Int, @in Int) -> ()
// CHECK:   store {{%.*}} to [[T3]] :
  array[0] = id_int

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_TFV10addressors6CArraylu9subscriptFSiQ_ :
// CHECK:   [[T2:%.*]] = apply [[T1]]<Int -> Int>({{%.*}}, [[T0]])
// CHECK:   [[T3:%.*]] = struct_extract [[T2]] : $UnsafePointer<Int -> Int>, #UnsafePointer._rawValue
// CHECK:   [[T4:%.*]] = pointer_to_address [[T3]] : $Builtin.RawPointer to $*@callee_owned (@out Int, @in Int) -> ()
// CHECK:   [[T5:%.*]] = load [[T4]]
  return array[1](5)
}

// rdar://17270560, redux
struct D : Subscriptable {
  subscript(i: Int) -> Int {
    get { return i }
    unsafeMutableAddress { return nil }
  }
}
// Setter.
// CHECK: sil hidden [transparent] @_TFV10addressors1Ds9subscriptFSiSi
// CHECK: bb0([[VALUE:%.*]] : $Int, [[I:%.*]] : $Int, [[SELF:%.*]] : $*D):
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFSiSi
// CHECK:   [[PTR:%.*]] = apply [[T0]]([[I]], [[SELF]])
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[VALUE]] to [[ADDR]] : $*Int

// materializeForSet.
// CHECK: sil hidden [transparent] @_TFV10addressors1Dm9subscriptFSiSi
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[I:%.*]] : $Int, [[SELF:%.*]] : $*D):
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFSiSi
// CHECK:   [[PTR:%.*]] = apply [[T0]]([[I]], [[SELF]])
// CHECK:   [[ADDR:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int>,
// CHECK:   [[TMP:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>
// CHECK:   inject_enum_addr [[TMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>, #Optional.None
// CHECK:   [[T1:%.*]] = load [[TMP]]#1
// CHECK:   [[T2:%.*]] = tuple ([[ADDR]] : $Builtin.RawPointer, [[T1]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout D, @thick D.Type) -> ()>)
// CHECK:   return [[T2]] :

func make_int() -> Int { return 0 }
func take_int_inout(inout value: Int) {}

// CHECK: sil hidden @_TF10addressors6test_dFRVS_1DSi : $@thin (@inout D) -> Int
// CHECK: bb0([[ARRAY:%.*]] : $*D):
func test_d(inout array: D) -> Int {
// CHECK:   [[T0:%.*]] = function_ref @_TF10addressors8make_intFT_Si
// CHECK:   [[V:%.*]] = apply [[T0]]()
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFSiSi
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[V]] to [[ADDR]] : $*Int
  array[0] = make_int()

// CHECK:   [[FN:%.*]] = function_ref @_TF10addressors14take_int_inoutFRSiT_
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Dau9subscriptFSiSi
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK:   apply [[FN]]([[ADDR]])
  take_int_inout(&array[1])

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_TFV10addressors1Dg9subscriptFSiSi 
// CHECK:   [[T2:%.*]] = apply [[T1]]({{%.*}}, [[T0]])
// CHECK:   return [[T2]]
  return array[2]
}

struct E {
  var value: Int {
    unsafeAddress { return nil }
    nonmutating unsafeMutableAddress { return nil }
  }
}

// CHECK: sil hidden @_TF10addressors6test_eFVS_1ET_
// CHECK: bb0([[E:%.*]] : $E):
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors1Eau5valueSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[E]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]]
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]]
// CHECK:   store {{%.*}} to [[T3]] : $*Int
func test_e(e: E) {
  e.value = 0
}

class F {
  var data: UnsafeMutablePointer<Int> = UnsafeMutablePointer.alloc(100)

  final var value: Int {
    addressWithOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}

// CHECK: sil hidden @_TFC10addressors1Flo5valueSi : $@cc(method) @thin (@owned F) -> @owned (UnsafePointer<Int>, Builtin.NativeObject) {
// CHECK: sil hidden @_TFC10addressors1Fao5valueSi : $@cc(method) @thin (@owned F) -> @owned (UnsafeMutablePointer<Int>, Builtin.NativeObject) {

func test_f0(f: F) -> Int {
  return f.value
}
// CHECK: sil hidden @_TF10addressors7test_f0FCS_1FSi : $@thin (@owned F) -> Int {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   strong_retain [[SELF]] : $F
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Flo5valueSi : $@cc(method) @thin (@owned F) -> @owned (UnsafePointer<Int>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F
// CHECK:   return [[VALUE]] : $Int

func test_f1(f: F) {
  f.value = 14
}
// CHECK: sil hidden @_TF10addressors7test_f1FCS_1FT_ : $@thin (@owned F) -> () {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Word, 14
// CHECK:   [[VALUE:%.*]] = struct $Int ([[T0]] : $Builtin.Word)
// CHECK:   strong_retain [[SELF]] : $F
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Fao5valueSi : $@cc(method) @thin (@owned F) -> @owned (UnsafeMutablePointer<Int>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F

class G {
  var data: UnsafeMutablePointer<Int> = UnsafeMutablePointer.alloc(100)

  var value: Int {
    addressWithOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}
// CHECK: sil hidden [transparent] @_TFC10addressors1Gg5valueSi : $@cc(method) @thin (@owned G) -> Int {
// CHECK: bb0([[SELF:%0]] : $G):
// CHECK:   strong_retain [[SELF]]
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Glo5valueSi : $@cc(method) @thin (@owned G) -> @owned (UnsafePointer<Int>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $G
// CHECK:   return [[VALUE]] : $Int

// CHECK: sil hidden [transparent] @_TFC10addressors1Gs5valueSi : $@cc(method) @thin (Int, @owned G) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int, [[SELF:%1]] : $G):
// CHECK:   strong_retain [[SELF]] : $G
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Gao5valueSi : $@cc(method) @thin (@owned G) -> @owned (UnsafeMutablePointer<Int>, Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $G

//   materializeForSet for G.value
// CHECK: sil hidden [transparent] @_TFC10addressors1Gm5valueSi : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @owned G) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $G):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Gao5valueSi : $@cc(method) @thin (@owned G) -> @owned (UnsafeMutablePointer<Int>, Builtin.NativeObject)
// CHECK:   strong_retain [[SELF]] : $G
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 1
// CHECK:   [[TUPLE:%.*]] = tuple ([[T1]] : $UnsafeMutablePointer<Int>, [[T2]] : $Builtin.NativeObject)
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   retain_value [[TUPLE]]
// CHECK:   [[T3:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 1
// CHECK:   store [[T3]] to [[T2]] : $*Builtin.NativeObject
//   Pull out the address.
// CHECK:   [[T0:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int>, Builtin.NativeObject), 0
// CHECK:   [[PTR:%.*]] = struct_extract [[T0]] :
//   Set up the callback.
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>
// CHECK:   [[T0:%.*]] = init_enum_data_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[T1:%.*]] = function_ref @_TFFC10addressors1Gm5valueSiU_FTBpRBBRS0_MS0__T_ :
// CHECK:   store [[T1]] to [[T0]] :
// CHECK:   inject_enum_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[CALLBACK:%.*]] = load [[TEMP]]#1 :
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout G, @thick G.Type) -> ()>)
// CHECK:   release_value [[TUPLE]]
// CHECK:   strong_release [[SELF]]
// CHECK:   return [[RESULT]]

//   materializeForSet callback for G.value
// CHECK: sil @_TFFC10addressors1Gm5valueSiU_FTBpRBBRS0_MS0__T_ : $@thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout G, @thick G.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*G, [[SELFTYPE:%3]] : $@thick G.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   [[OWNER:%.*]] = load [[T2]]
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   dealloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer

class H {
  var data: UnsafeMutablePointer<Int> = UnsafeMutablePointer.alloc(100)

  final var value: Int {
    addressWithPinnedOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}

// CHECK: sil hidden @_TFC10addressors1Hlp5valueSi : $@cc(method) @thin (@owned H) -> @owned (UnsafePointer<Int>, Optional<Builtin.NativeObject>) {
// CHECK: sil hidden @_TFC10addressors1Hap5valueSi : $@cc(method) @thin (@owned H) -> @owned (UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>) {

func test_h0(f: H) -> Int {
  return f.value
}
// CHECK: sil hidden @_TF10addressors7test_h0FCS_1HSi : $@thin (@owned H) -> Int {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   strong_retain [[SELF]] : $H
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Hlp5valueSi : $@cc(method) @thin (@owned H) -> @owned (UnsafePointer<Int>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H
// CHECK:   return [[VALUE]] : $Int

func test_h1(f: H) {
  f.value = 14
}
// CHECK: sil hidden @_TF10addressors7test_h1FCS_1HT_ : $@thin (@owned H) -> () {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Word, 14
// CHECK:   [[VALUE:%.*]] = struct $Int ([[T0]] : $Builtin.Word)
// CHECK:   strong_retain [[SELF]] : $H
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Hap5valueSi : $@cc(method) @thin (@owned H) -> @owned (UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H

class I {
  var data: UnsafeMutablePointer<Int> = UnsafeMutablePointer.alloc(100)

  var value: Int {
    addressWithPinnedOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}
// CHECK: sil hidden [transparent] @_TFC10addressors1Ig5valueSi : $@cc(method) @thin (@owned I) -> Int {
// CHECK: bb0([[SELF:%0]] : $I):
// CHECK:   strong_retain [[SELF]]
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Ilp5valueSi : $@cc(method) @thin (@owned I) -> @owned (UnsafePointer<Int>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $I
// CHECK:   return [[VALUE]] : $Int

// CHECK: sil hidden [transparent] @_TFC10addressors1Is5valueSi : $@cc(method) @thin (Int, @owned I) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int, [[SELF:%1]] : $I):
// CHECK:   strong_retain [[SELF]] : $I
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Iap5valueSi : $@cc(method) @thin (@owned I) -> @owned (UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $I

// CHECK: sil hidden [transparent] @_TFC10addressors1Im5valueSi : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @owned I) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $I):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_TFC10addressors1Iap5valueSi : $@cc(method) @thin (@owned I) -> @owned (UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>)
// CHECK:   strong_retain [[SELF]] : $I
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[TUPLE:%.*]] = tuple ([[T1]] : $UnsafeMutablePointer<Int>, [[T2]] : $Optional<Builtin.NativeObject>)
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   retain_value [[TUPLE]]
// CHECK:   [[T3:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 1
// CHECK:   store [[T3]] to [[T2]] : $*Optional<Builtin.NativeObject>
//   Pull out the address.
// CHECK:   [[T0:%.*]] = tuple_extract [[TUPLE]] : $(UnsafeMutablePointer<Int>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[PTR:%.*]] = struct_extract [[T0]] :
//   Set up the callback.
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>
// CHECK:   [[T0:%.*]] = init_enum_data_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[T1:%.*]] = function_ref @_TFFC10addressors1Im5valueSiU_FTBpRBBRS0_MS0__T_ :
// CHECK:   store [[T1]] to [[T0]] :
// CHECK:   inject_enum_addr [[TEMP]]#1 : $*Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>, #Optional.Some!enumelt.1
// CHECK:   [[CALLBACK:%.*]] = load [[TEMP]]#1 :
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout I, @thick I.Type) -> ()>)
// CHECK:   release_value [[TUPLE]]
// CHECK:   strong_release [[SELF]]
// CHECK:   return [[RESULT]]

//   materializeForSet callback for I.value
// CHECK: sil @_TFFC10addressors1Im5valueSiU_FTBpRBBRS0_MS0__T_ : $@thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout I, @thick I.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*I, [[SELFTYPE:%3]] : $@thick I.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]]
// CHECK:   [[T2:%.*]] = pointer_to_address [[T1]]
// CHECK:   [[OWNER:%.*]] = load [[T2]]
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   dealloc_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer

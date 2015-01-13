// RUN: %swift -emit-sil %s | FileCheck %s

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

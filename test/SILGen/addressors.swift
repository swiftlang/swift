// RUN: %swift -emit-sil %s | FileCheck %s

struct A {
  var base: UnsafeMutablePointer<Int> = nil

  subscript(index: Int) -> Int {
    address {
      return UnsafePointer(base)
    }
    mutableAddress {
      return base
    }
  }
}

// CHECK-LABEL: sil hidden @_TFV10addressors1Al9subscriptFSiSi : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: bb0([[INDEX:%.*]] : $Int, [[SELF:%.*]] : $A):
// CHECK:   [[BASE:%.*]] = struct_extract [[SELF]] : $A, #A.base
// CHECK:   [[T0:%.*]] = struct_extract [[BASE]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T1:%.*]] = struct $UnsafePointer<Int> ([[T0]] : $Builtin.RawPointer)
// CHECK:   return [[T1]] : $UnsafePointer<Int>

// CHECK-LABEL: sil hidden @_TFV10addressors1Aa9subscriptFSiSi : $@cc(method) @thin (Int, @inout A) -> UnsafeMutablePointer<Int>
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

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Al9subscriptFSiSi :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[AVAL]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK: [[Z:%.*]] = load [[T3]] : $*Int
  let z = a[10]

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aa9subscriptFSiSi :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]]#1)
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK: [[T4:%.*]] = struct_element_addr [[T3]] : $*Int, #Int.value
// CHECK: load [[T4]]
// CHECK: sadd_with_overflow_Word
// CHECK: store {{%.*}} to [[T3]]
  a[5] += z

// CHECK: [[T0:%.*]] = function_ref @_TFV10addressors1Aa9subscriptFSiSi :
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
// CHECK: [[ACCESSOR:%.*]] = function_ref @_TFV10addressors1Al9subscriptFSiSi : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: [[PTR:%.*]] = apply [[ACCESSOR]]({{%.*}}, [[A]]) : $@cc(method) @thin (Int, A) -> UnsafePointer<Int>
// CHECK: [[T0:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK: [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Int
// CHECK: [[T2:%.*]] = load [[T1]] : $*Int
// CHECK: return [[T2]] : $Int
  return A()[0]
}

let uninitAddr = UnsafeMutablePointer<Int>.alloc(1)
var global: Int {
  address {
    return UnsafePointer(uninitAddr)
  }
// CHECK: sil hidden @_TF10addressorsl6globalSi : $@thin () -> UnsafePointer<Int> {
// CHECK:   [[T0:%.*]] = sil_global_addr @_Tv10addressors10uninitAddrGVSs20UnsafeMutablePointerSi_ : $*UnsafeMutablePointer<Int>
// CHECK:   [[T1:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = struct $UnsafePointer<Int> ([[T2]] : $Builtin.RawPointer)
// CHECK:   return [[T3]] : $UnsafePointer<Int>
}

func test_global() -> Int {
  return global
}
// CHECK: sil hidden @_TF10addressors11test_globalFT_Si : $@thin () -> Int {
// CHECK:   [[T0:%.*]] = function_ref @_TF10addressorsl6globalSi : $@thin () -> UnsafePointer<Int>
// CHECK:   [[T1:%.*]] = apply [[T0]]() : $@thin () -> UnsafePointer<Int>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int>, #UnsafePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T4:%.*]] = load [[T3]] : $*Int
// CHECK:   return [[T4]] : $Int

struct CArray<T> {
  var storage: UnsafeMutablePointer<T> = nil
  subscript(index: Int) -> T {
    address { return UnsafePointer(storage) + index }
    mutableAddress { return storage + index }
  }
}

func id_int(i: Int) -> Int { return i }

// CHECK: sil hidden @_TF10addressors11test_carrayFRGVS_6CArrayFSiSi_Si : $@thin (@inout CArray<Int -> Int>) -> Int {
// CHECK: bb0([[ARRAY:%.*]] : $*CArray<Int -> Int>):
func test_carray(inout array: CArray<Int -> Int>) -> Int {
// CHECK:   [[T0:%.*]] = function_ref @_TFV10addressors6CArraya9subscriptFSiQ_ :
// CHECK:   [[T1:%.*]] = apply [[T0]]<Int -> Int>({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int -> Int>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to $*@callee_owned (@out Int, @in Int) -> ()
// CHECK:   store {{%.*}} to [[T3]] :
  array[0] = id_int

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_TFV10addressors6CArrayl9subscriptFSiQ_ :
// CHECK:   [[T2:%.*]] = apply [[T1]]<Int -> Int>({{%.*}}, [[T0]])
// CHECK:   [[T3:%.*]] = struct_extract [[T2]] : $UnsafePointer<Int -> Int>, #UnsafePointer._rawValue
// CHECK:   [[T4:%.*]] = pointer_to_address [[T3]] : $Builtin.RawPointer to $*@callee_owned (@out Int, @in Int) -> ()
// CHECK:   [[T5:%.*]] = load [[T4]]
  return array[1](5)
}

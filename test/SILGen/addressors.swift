// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -emit-silgen %s | %FileCheck %s -check-prefix=SILGEN

import Swift

func someValidPointer<T>() -> UnsafePointer<T> { fatalError() }
func someValidPointer<T>() -> UnsafeMutablePointer<T> { fatalError() }

struct A {
  var base: UnsafeMutablePointer<Int32> = someValidPointer()

  subscript(index: Int32) -> Int32 {
    unsafeAddress {
      return UnsafePointer(base)
    }
    unsafeMutableAddress {
      return base
    }
  }
}

// CHECK-LABEL: sil hidden @_T010addressors1AV9subscripts5Int32VAFcflu : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $A):
// CHECK:   [[BASE:%.*]] = struct_extract [[SELF]] : $A, #A.base
// CHECK:   [[T0:%.*]] = struct_extract [[BASE]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T1:%.*]] = struct $UnsafePointer<Int32> ([[T0]] : $Builtin.RawPointer)
// CHECK:   return [[T1]] : $UnsafePointer<Int32>

// CHECK-LABEL: sil hidden @_T010addressors1AV9subscripts5Int32VAFcfau : $@convention(method) (Int32, @inout A) -> UnsafeMutablePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*A):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*A, #A.base
// CHECK:   [[BASE:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   return [[BASE]] : $UnsafeMutablePointer<Int32>

// CHECK-LABEL: sil hidden @_T010addressors5test0yyF : $@convention(thin) () -> () {
func test0() {
// CHECK: [[A:%.*]] = alloc_stack $A
// CHECK: [[T0:%.*]] = function_ref @_T010addressors1AV{{[_0-9a-zA-Z]*}}fC
// CHECK: [[T1:%.*]] = metatype $@thin A.Type
// CHECK: [[AVAL:%.*]] = apply [[T0]]([[T1]]) 
// CHECK: store [[AVAL]] to [[A]]
  var a = A()

// CHECK: [[T0:%.*]] = function_ref @_T010addressors1AV9subscripts5Int32VAFcflu :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[AVAL]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[Z:%.*]] = load [[T3]] : $*Int32
  let z = a[10]

// CHECK: [[T0:%.*]] = function_ref @_T010addressors1AV9subscripts5Int32VAFcfau :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: load
// CHECK: sadd_with_overflow_Int{{32|64}}
// CHECK: store {{%.*}} to [[T3]]
  a[5] += z

// CHECK: [[T0:%.*]] = function_ref @_T010addressors1AV9subscripts5Int32VAFcfau :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[A]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: store {{%.*}} to [[T3]]
  a[3] = 6
}

// CHECK-LABEL: sil hidden @_T010addressors5test1s5Int32VyF : $@convention(thin) () -> Int32
func test1() -> Int32 {
// CHECK: [[CTOR:%.*]] = function_ref @_T010addressors1AV{{[_0-9a-zA-Z]*}}fC
// CHECK: [[T0:%.*]] = metatype $@thin A.Type
// CHECK: [[A:%.*]] = apply [[CTOR]]([[T0]]) : $@convention(method) (@thin A.Type) -> A
// CHECK: [[ACCESSOR:%.*]] = function_ref @_T010addressors1AV9subscripts5Int32VAFcflu : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[PTR:%.*]] = apply [[ACCESSOR]]({{%.*}}, [[A]]) : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[T0:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[T2:%.*]] = load [[T1]] : $*Int32
// CHECK: return [[T2]] : $Int32
  return A()[0]
}

let uninitAddr = UnsafeMutablePointer<Int32>.allocate(capacity: 1)
var global: Int32 {
  unsafeAddress {
    return UnsafePointer(uninitAddr)
  }
// CHECK: sil hidden @_T010addressors6globals5Int32Vflu : $@convention(thin) () -> UnsafePointer<Int32> {
// CHECK:   [[T0:%.*]] = global_addr @_T010addressors10uninitAddrSpys5Int32VGv : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T1:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = struct $UnsafePointer<Int32> ([[T2]] : $Builtin.RawPointer)
// CHECK:   return [[T3]] : $UnsafePointer<Int32>
}

func test_global() -> Int32 {
  return global
}
// CHECK: sil hidden @_T010addressors11test_globals5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors6globals5Int32Vflu : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK:   [[T1:%.*]] = apply [[T0]]() : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T4:%.*]] = load [[T3]] : $*Int32
// CHECK:   return [[T4]] : $Int32

// Test that having generated trivial accessors for something because
// of protocol conformance doesn't force us down inefficient access paths.
protocol Subscriptable {
  subscript(i: Int32) -> Int32 { get set }
}

struct B : Subscriptable {
  subscript(i: Int32) -> Int32 {
    unsafeAddress { return someValidPointer() }
    unsafeMutableAddress { return someValidPointer() }
  }
}

// CHECK: sil hidden @_T010addressors6test_ByAA1BVzF : $@convention(thin) (@inout B) -> () {
// CHECK: bb0([[B:%.*]] : $*B):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 0
// CHECK:   [[INDEX:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   [[RHS:%.*]] = integer_literal $Builtin.Int32, 7
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors1BV9subscripts5Int32VAFcfau
// CHECK:   [[PTR:%.*]] = apply [[T0]]([[INDEX]], [[B]])
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// Accept either of struct_extract+load or load+struct_element_addr.
// CHECK:   load
// CHECK:   [[T1:%.*]] = builtin "or_Int32"
// CHECK:   [[T2:%.*]] = struct $Int32 ([[T1]] : $Builtin.Int32)
// CHECK:   store [[T2]] to [[ADDR]] : $*Int32
func test_B(_ b: inout B) {
  b[0] |= 7
}

// Test that we handle abstraction difference.
struct CArray<T> {
  var storage: UnsafeMutablePointer<T>
  subscript(index: Int) -> T {
    unsafeAddress { return UnsafePointer(storage) + index }
    unsafeMutableAddress { return storage + index }
  }
}

func id_int(_ i: Int32) -> Int32 { return i }

// CHECK-LABEL: sil hidden @_T010addressors11test_carrays5Int32VAA6CArrayVyAdDcGzF : $@convention(thin) (@inout CArray<(Int32) -> Int32>) -> Int32 {
// CHECK: bb0([[ARRAY:%.*]] : $*CArray<(Int32) -> Int32>):
func test_carray(_ array: inout CArray<(Int32) -> Int32>) -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors6CArrayV9subscriptxSicfau :
// CHECK:   [[T1:%.*]] = apply [[T0]]<(Int32) -> Int32>({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<(Int32) -> Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*@callee_owned (@in Int32) -> @out Int32
// CHECK:   store {{%.*}} to [[T3]] :
  array[0] = id_int

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_T010addressors6CArrayV9subscriptxSicflu :
// CHECK:   [[T2:%.*]] = apply [[T1]]<(Int32) -> Int32>({{%.*}}, [[T0]])
// CHECK:   [[T3:%.*]] = struct_extract [[T2]] : $UnsafePointer<(Int32) -> Int32>, #UnsafePointer._rawValue
// CHECK:   [[T4:%.*]] = pointer_to_address [[T3]] : $Builtin.RawPointer to [strict] $*@callee_owned (@in Int32) -> @out Int32
// CHECK:   [[T5:%.*]] = load [[T4]]
  return array[1](5)
}

// rdar://17270560, redux
struct D : Subscriptable {
  subscript(i: Int32) -> Int32 {
    get { return i }
    unsafeMutableAddress { return someValidPointer() }
  }
}
// Setter.
// SILGEN-LABEL: sil hidden [transparent] @_T010addressors1DV9subscripts5Int32VAFcfs
// SILGEN: bb0([[VALUE:%.*]] : $Int32, [[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   debug_value [[VALUE]] : $Int32
// SILGEN:   debug_value [[I]] : $Int32
// SILGEN:   debug_value_addr [[SELF]]
// SILGEN:   [[T0:%.*]] = function_ref @_T010addressors1DV9subscripts5Int32VAFcfau{{.*}}
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[SELF]])
// SILGEN:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// SILGEN:   assign [[VALUE]] to [[ADDR]] : $*Int32

// materializeForSet.
// SILGEN: sil hidden [transparent] @_T010addressors1DV9subscripts5Int32VAFcfm
// SILGEN: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   [[T0:%.*]] = function_ref @_T010addressors1DV9subscripts5Int32VAFcfau
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[SELF]])
// SILGEN:   [[ADDR_TMP:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[ADDR:%.*]] = pointer_to_address [[ADDR_TMP]]
// SILGEN:   [[PTR:%.*]] = address_to_pointer [[ADDR]]
// SILGEN:   [[OPT:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.none!enumelt
// SILGEN:   [[T2:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[OPT]] : $Optional<Builtin.RawPointer>)
// SILGEN:   return [[T2]] :

func make_int() -> Int32 { return 0 }
func take_int_inout(_ value: inout Int32) {}

// CHECK-LABEL: sil hidden @_T010addressors6test_ds5Int32VAA1DVzF : $@convention(thin) (@inout D) -> Int32
// CHECK: bb0([[ARRAY:%.*]] : $*D):
func test_d(_ array: inout D) -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors8make_ints5Int32VyF
// CHECK:   [[V:%.*]] = apply [[T0]]()
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors1DV9subscripts5Int32VAFcfau
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   store [[V]] to [[ADDR]] : $*Int32
  array[0] = make_int()

// CHECK:   [[FN:%.*]] = function_ref @_T010addressors14take_int_inoutys5Int32VzF
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors1DV9subscripts5Int32VAFcfau
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[ARRAY]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   apply [[FN]]([[ADDR]])
  take_int_inout(&array[1])

// CHECK:   [[T0:%.*]] = load [[ARRAY]]
// CHECK:   [[T1:%.*]] = function_ref @_T010addressors1DV9subscripts5Int32VAFcfg 
// CHECK:   [[T2:%.*]] = apply [[T1]]({{%.*}}, [[T0]])
// CHECK:   return [[T2]]
  return array[2]
}

struct E {
  var value: Int32 {
    unsafeAddress { return someValidPointer() }
    nonmutating unsafeMutableAddress { return someValidPointer() }
  }
}

// CHECK-LABEL: sil hidden @_T010addressors6test_eyAA1EVF
// CHECK: bb0([[E:%.*]] : $E):
// CHECK:   [[T0:%.*]] = function_ref @_T010addressors1EV5values5Int32Vfau
// CHECK:   [[T1:%.*]] = apply [[T0]]([[E]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]]
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]]
// CHECK:   store {{%.*}} to [[T3]] : $*Int32
func test_e(_ e: E) {
  e.value = 0
}

class F {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.allocate(capacity: 100)

  final var value: Int32 {
    addressWithNativeOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithNativeOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}

// CHECK: sil hidden @_T010addressors1FC5values5Int32Vflo : $@convention(method) (@guaranteed F) -> (UnsafePointer<Int32>, @owned Builtin.NativeObject) {
// CHECK: sil hidden @_T010addressors1FC5values5Int32Vfao : $@convention(method) (@guaranteed F) -> (UnsafeMutablePointer<Int32>, @owned Builtin.NativeObject) {

func test_f0(_ f: F) -> Int32 {
  return f.value
}
// CHECK: sil hidden @_T010addressors7test_f0s5Int32VAA1FCF : $@convention(thin) (@owned F) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1FC5values5Int32Vflo : $@convention(method) (@guaranteed F) -> (UnsafePointer<Int32>, @owned Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F
// CHECK:   return [[VALUE]] : $Int32

func test_f1(_ f: F) {
  f.value = 14
}
// CHECK: sil hidden @_T010addressors7test_f1yAA1FCF : $@convention(thin) (@owned F) -> () {
// CHECK: bb0([[SELF:%0]] : $F):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 14
// CHECK:   [[VALUE:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1FC5values5Int32Vfao : $@convention(method) (@guaranteed F) -> (UnsafeMutablePointer<Int32>, @owned Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   strong_release [[SELF]] : $F

class G {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.allocate(capacity: 100)

  var value: Int32 {
    addressWithNativeOwner {
      return (UnsafePointer(data), Builtin.castToNativeObject(self))
    }
    mutableAddressWithNativeOwner {
      return (data, Builtin.castToNativeObject(self))
    }
  }
}
// CHECK: sil hidden [transparent] @_T010addressors1GC5values5Int32Vfg : $@convention(method) (@guaranteed G) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $G):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1GC5values5Int32Vflo : $@convention(method) (@guaranteed G) -> (UnsafePointer<Int32>, @owned Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   return [[VALUE]] : $Int32

// CHECK: sil hidden [transparent] @_T010addressors1GC5values5Int32Vfs : $@convention(method) (Int32, @guaranteed G) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int32, [[SELF:%1]] : $G):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1GC5values5Int32Vfao : $@convention(method) (@guaranteed G) -> (UnsafeMutablePointer<Int32>, @owned Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Builtin.NativeObject
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject

//   materializeForSet for G.value
// CHECK-LABEL: sil hidden [transparent] @_T010addressors1GC5values5Int32Vfm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed G) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $G):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1GC5values5Int32Vfao : $@convention(method) (@guaranteed G) -> (UnsafeMutablePointer<Int32>, @owned Builtin.NativeObject)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Builtin.NativeObject), 1
//   Get the address.
// CHECK:   [[PTR:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[ADDR_TMP:%.*]] = pointer_to_address [[PTR]]
// CHECK:   [[ADDR:%.*]] = mark_dependence [[ADDR_TMP]] : $*Int32 on [[T2]]
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   store [[T2]] to [[T0]] : $*Builtin.NativeObject
// CHECK:   [[PTR:%.*]] = address_to_pointer [[ADDR]]
//   Set up the callback.
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_T010addressors1GC5values5Int32VfmytfU_ :
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[RESULT]]

//   materializeForSet callback for G.value
// CHECK-LABEL: sil hidden [transparent] @_T010addressors1GC5values5Int32VfmytfU_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout G, @thick G.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*G, [[SELFTYPE:%3]] : $@thick G.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[OWNER:%.*]] = load [[T0]]
// CHECK:   strong_release [[OWNER]] : $Builtin.NativeObject
// CHECK:   dealloc_value_buffer $Builtin.NativeObject in [[STORAGE]] : $*Builtin.UnsafeValueBuffer

class H {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.allocate(capacity: 100)

  final var value: Int32 {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}

// CHECK-LABEL: sil hidden @_T010addressors1HC5values5Int32Vflp : $@convention(method) (@guaranteed H) -> (UnsafePointer<Int32>, @owned Optional<Builtin.NativeObject>) {
// CHECK-LABEL: sil hidden @_T010addressors1HC5values5Int32VfaP : $@convention(method) (@guaranteed H) -> (UnsafeMutablePointer<Int32>, @owned Optional<Builtin.NativeObject>) {

func test_h0(_ f: H) -> Int32 {
  return f.value
}
// CHECK-LABEL: sil hidden @_T010addressors7test_h0s5Int32VAA1HCF : $@convention(thin) (@owned H) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1HC5values5Int32Vflp : $@convention(method) (@guaranteed H) -> (UnsafePointer<Int32>, @owned Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H
// CHECK:   return [[VALUE]] : $Int32

func test_h1(_ f: H) {
  f.value = 14
}
// CHECK: sil hidden @_T010addressors7test_h1yAA1HCF : $@convention(thin) (@owned H) -> () {
// CHECK: bb0([[SELF:%0]] : $H):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 14
// CHECK:   [[VALUE:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1HC5values5Int32VfaP : $@convention(method) (@guaranteed H) -> (UnsafeMutablePointer<Int32>, @owned Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   strong_release [[SELF]] : $H

class I {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.allocate(capacity: 100)

  var value: Int32 {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}
// CHECK-LABEL: sil hidden [transparent] @_T010addressors1IC5values5Int32Vfg : $@convention(method) (@guaranteed I) -> Int32 {
// CHECK: bb0([[SELF:%0]] : $I):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1IC5values5Int32Vflp : $@convention(method) (@guaranteed I) -> (UnsafePointer<Int32>, @owned Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   [[VALUE:%.*]] = load [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   return [[VALUE]] : $Int32

// CHECK-LABEL: sil hidden [transparent] @_T010addressors1IC5values5Int32Vfs : $@convention(method) (Int32, @guaranteed I) -> () {
// CHECK: bb0([[VALUE:%0]] : $Int32, [[SELF:%1]] : $I):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1IC5values5Int32VfaP : $@convention(method) (@guaranteed I) -> (UnsafeMutablePointer<Int32>, @owned Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[PTR:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[OWNER:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[T2:%.*]] = mark_dependence [[T1]] : $*Int32 on [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK:   store [[VALUE]] to [[T2]] : $*Int32
// CHECK:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>

// CHECK-LABEL: sil hidden [transparent] @_T010addressors1IC5values5Int32Vfm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed I) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $I):
//   Call the addressor.
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @_T010addressors1IC5values5Int32VfaP : $@convention(method) (@guaranteed I) -> (UnsafeMutablePointer<Int32>, @owned Optional<Builtin.NativeObject>)
// CHECK:   [[T0:%.*]] = apply [[ADDRESSOR]]([[SELF]])
// CHECK:   [[T1:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 0
// CHECK:   [[T2:%.*]] = tuple_extract [[T0]] : $(UnsafeMutablePointer<Int32>, Optional<Builtin.NativeObject>), 1
//   Pull out the address.
// CHECK:   [[PTR:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[ADDR_TMP:%.*]] = pointer_to_address [[PTR]]
// CHECK:   [[ADDR:%.*]] = mark_dependence [[ADDR_TMP]] : $*Int32 on [[T2]]
//   Initialize the callback storage with the owner.
// CHECK:   [[T0:%.*]] = alloc_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   store [[T2]] to [[T0]] : $*Optional<Builtin.NativeObject>
// CHECK:   [[PTR:%.*]] = address_to_pointer [[ADDR]]
//   Set up the callback.
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_T010addressors1IC5values5Int32VfmytfU_ :
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]] 
//   Epilogue.
// CHECK:   [[RESULT:%.*]] = tuple ([[PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[RESULT]]

//   materializeForSet callback for I.value
// CHECK-LABEL: sil hidden [transparent] @_T010addressors1IC5values5Int32VfmytfU_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout I, @thick I.Type) -> () {
// CHECK: bb0([[BUFFER:%0]] : $Builtin.RawPointer, [[STORAGE:%1]] : $*Builtin.UnsafeValueBuffer, [[SELF:%2]] : $*I, [[SELFTYPE:%3]] : $@thick I.Type):
// CHECK:   [[T0:%.*]] = project_value_buffer $Optional<Builtin.NativeObject> in [[STORAGE]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[OWNER:%.*]] = load [[T0]]
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
  var data: UnsafeMutablePointer<RecMiddle> = UnsafeMutablePointer.allocate(capacity: 100)
  final var middle: RecMiddle {
    addressWithPinnedNativeOwner {
      return (UnsafePointer(data), Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
    mutableAddressWithPinnedNativeOwner {
      return (data, Builtin.tryPin(Builtin.castToNativeObject(self)))
    }
  }
}
func test_rec(_ outer: RecOuter) -> Int32 {
  return outer.middle.inner[0]
}
// This uses the mutable addressor.
// CHECK-LABEL: sil hidden @_T010addressors8test_recs5Int32VAA8RecOuterCF : $@convention(thin) (@owned RecOuter) -> Int32 {
// CHECK:   function_ref @_T010addressors8RecOuterC6middleAA0B6MiddleVfaP
// CHECK:   struct_element_addr {{.*}} : $*RecMiddle, #RecMiddle.inner
// CHECK:   function_ref @_T010addressors8RecInnerV9subscripts5Int32VAFcfg 

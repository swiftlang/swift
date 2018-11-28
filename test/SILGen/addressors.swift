
// RUN: %target-swift-emit-sil -enable-sil-ownership -parse-stdlib %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s | %FileCheck %s -check-prefix=SILGEN
// RUN: %target-swift-emit-ir -enable-sil-ownership -parse-stdlib %s

// This test includes some calls to transparent stdlib functions.
// We pattern match for the absence of access markers in the inlined code.
// REQUIRES: optimized_stdlib

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

  static var staticProp: Int32 {
    unsafeAddress {
      // Just don't trip up the verifier.
      fatalError()
    }
  }
}

// CHECK-LABEL: sil hidden @$s10addressors1AVys5Int32VAEcilu : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $A):
// CHECK:   [[BASE:%.*]] = struct_extract [[SELF]] : $A, #A.base
// CHECK:   [[T0:%.*]] = struct_extract [[BASE]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T1:%.*]] = struct $UnsafePointer<Int32> ([[T0]] : $Builtin.RawPointer)
// CHECK:   return [[T1]] : $UnsafePointer<Int32>

// CHECK-LABEL: sil hidden @$s10addressors1AVys5Int32VAEciau : $@convention(method) (Int32, @inout A) -> UnsafeMutablePointer<Int32>
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*A):
// CHECK:   [[READ:%.*]] = begin_access [read] [static] [[SELF]] : $*A
// CHECK:   [[T0:%.*]] = struct_element_addr [[READ]] : $*A, #A.base
// CHECK:   [[BASE:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   return [[BASE]] : $UnsafeMutablePointer<Int32>

// CHECK-LABEL: sil hidden @$s10addressors5test0yyF : $@convention(thin) () -> () {
func test0() {
// CHECK: [[A:%.*]] = alloc_stack $A
// CHECK: [[T1:%.*]] = metatype $@thin A.Type
// CHECK: [[T0:%.*]] = function_ref @$s10addressors1AV{{[_0-9a-zA-Z]*}}fC
// CHECK: [[AVAL:%.*]] = apply [[T0]]([[T1]]) 
// CHECK: store [[AVAL]] to [[A]]
  var a = A()

// CHECK: [[T0:%.*]] = function_ref @$s10addressors1AVys5Int32VAEcilu :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[AVAL]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unsafe] [[T3]] : $*Int32
// CHECK: [[Z:%.*]] = load [[ACCESS]] : $*Int32
  let z = a[10]

// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[A]] : $*A
// CHECK: [[T0:%.*]] = function_ref @$s10addressors1AVys5Int32VAEciau :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[WRITE]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[T3]] : $*Int32
// CHECK: load
// CHECK: sadd_with_overflow_Int{{32|64}}
// CHECK: store {{%.*}} to [[ACCESS]]
  a[5] += z

// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] [[A]] : $*A
// CHECK: [[T0:%.*]] = function_ref @$s10addressors1AVys5Int32VAEciau :
// CHECK: [[T1:%.*]] = apply [[T0]]({{%.*}}, [[WRITE]])
// CHECK: [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[T3]] : $*Int32
// CHECK: store {{%.*}} to [[ACCESS]]
  a[3] = 6
}

// CHECK-LABEL: sil hidden @$s10addressors5test1s5Int32VyF : $@convention(thin) () -> Int32
func test1() -> Int32 {
// CHECK: [[T0:%.*]] = metatype $@thin A.Type
// CHECK: [[CTOR:%.*]] = function_ref @$s10addressors1AV{{[_0-9a-zA-Z]*}}fC
// CHECK: [[A:%.*]] = apply [[CTOR]]([[T0]]) : $@convention(method) (@thin A.Type) -> A
// CHECK: [[ACCESSOR:%.*]] = function_ref @$s10addressors1AVys5Int32VAEcilu : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[PTR:%.*]] = apply [[ACCESSOR]]({{%.*}}, [[A]]) : $@convention(method) (Int32, A) -> UnsafePointer<Int32>
// CHECK: [[T0:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK: [[T1:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unsafe] [[T1]] : $*Int32
// CHECK: [[T2:%.*]] = load [[ACCESS]] : $*Int32
// CHECK: return [[T2]] : $Int32
  return A()[0]
}

let uninitAddr = UnsafeMutablePointer<Int32>.allocate(capacity: 1)
var global: Int32 {
  unsafeAddress {
    return UnsafePointer(uninitAddr)
  }
// CHECK: sil hidden @$s10addressors6globals5Int32Vvlu : $@convention(thin) () -> UnsafePointer<Int32> {
// CHECK:   [[T0:%.*]] = global_addr @$s10addressors10uninitAddrSpys5Int32VGvp : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T1:%.*]] = load [[T0]] : $*UnsafeMutablePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = struct $UnsafePointer<Int32> ([[T2]] : $Builtin.RawPointer)
// CHECK:   return [[T3]] : $UnsafePointer<Int32>
}

func test_global() -> Int32 {
  return global
}
// CHECK-LABEL: sil hidden @$s10addressors11test_globals5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors6globals5Int32Vvlu : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK:   [[T1:%.*]] = apply [[T0]]() : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unsafe] [[T3]] : $*Int32
// CHECK:   [[T4:%.*]] = load [[ACCESS]] : $*Int32
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

// CHECK-LABEL: sil hidden @$s10addressors6test_ByyAA1BVzF : $@convention(thin) (@inout B) -> () {
// CHECK: bb0([[B:%.*]] : $*B):
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int32, 0
// CHECK:   [[INDEX:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK:   [[RHS:%.*]] = integer_literal $Builtin.Int32, 7
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [static] [[B]] : $*B
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors1BVys5Int32VAEciau
// CHECK:   [[PTR:%.*]] = apply [[T0]]([[INDEX]], [[WRITE]])
// CHECK:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*Int32
// Accept either of struct_extract+load or load+struct_element_addr.
// CHECK:   load
// CHECK:   [[T1:%.*]] = builtin "or_Int32"
// CHECK:   [[T2:%.*]] = struct $Int32 ([[T1]] : $Builtin.Int32)
// CHECK:   store [[T2]] to [[ACCESS]] : $*Int32
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

// CHECK-LABEL: sil hidden @$s10addressors11test_carrayys5Int32VAA6CArrayVyA2DcGzF : $@convention(thin) (@inout CArray<(Int32) -> Int32>) -> Int32 {
// CHECK: bb0([[ARRAY:%.*]] : $*CArray<(Int32) -> Int32>):
func test_carray(_ array: inout CArray<(Int32) -> Int32>) -> Int32 {
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [static] [[ARRAY]] : $*CArray<(Int32) -> Int32>
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors6CArrayVyxSiciau :
// CHECK:   [[T1:%.*]] = apply [[T0]]<(Int32) -> Int32>({{%.*}}, [[WRITE]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<(Int32) -> Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*@callee_guaranteed (@in_guaranteed Int32) -> @out Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[T3]] : $*@callee_guaranteed (@in_guaranteed Int32) -> @out Int32
// CHECK:   store {{%.*}} to [[ACCESS]] :
  array[0] = id_int

// CHECK:   [[READ:%.*]] = begin_access [read] [static] [[ARRAY]] : $*CArray<(Int32) -> Int32>
// CHECK:   [[T0:%.*]] = load [[READ]]
// CHECK:   [[T1:%.*]] = function_ref @$s10addressors6CArrayVyxSicilu :
// CHECK:   [[T2:%.*]] = apply [[T1]]<(Int32) -> Int32>({{%.*}}, [[T0]])
// CHECK:   [[T3:%.*]] = struct_extract [[T2]] : $UnsafePointer<(Int32) -> Int32>, #UnsafePointer._rawValue
// CHECK:   [[T4:%.*]] = pointer_to_address [[T3]] : $Builtin.RawPointer to [strict] $*@callee_guaranteed (@in_guaranteed Int32) -> @out Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unsafe] [[T4]] : $*@callee_guaranteed (@in_guaranteed Int32) -> @out Int32
// CHECK:   [[T5:%.*]] = load [[ACCESS]]
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
// SILGEN-LABEL: sil hidden [transparent] @$s10addressors1DVys5Int32VAEcis
// SILGEN: bb0([[VALUE:%.*]] : $Int32, [[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   debug_value [[VALUE]] : $Int32
// SILGEN:   debug_value [[I]] : $Int32
// SILGEN:   debug_value_addr [[SELF]]
// SILGEN:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[SELF]] : $*D
// SILGEN:   [[T0:%.*]] = function_ref @$s10addressors1DVys5Int32VAEciau{{.*}}
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[ACCESS]])
// SILGEN:   [[T0:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[ADDR:%.*]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Int32
// SILGEN:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*Int32
// SILGEN:   assign [[VALUE]] to [[ACCESS]] : $*Int32

// SILGEN-LABEL: sil hidden [transparent] @$s10addressors1DVys5Int32VAEciM
// SILGEN: bb0([[I:%.*]] : $Int32, [[SELF:%.*]] : $*D):
// SILGEN:   [[SELF_ACCESS:%.*]] = begin_access [modify] [unknown] [[SELF]]
// SILGEN:   [[T0:%.*]] = function_ref @$s10addressors1DVys5Int32VAEciau
// SILGEN:   [[PTR:%.*]] = apply [[T0]]([[I]], [[SELF_ACCESS]])
// SILGEN:   [[ADDR_TMP:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>,
// SILGEN:   [[ADDR:%.*]] = pointer_to_address [[ADDR_TMP]]
// SILGEN:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]]
// SILGEN:   yield [[ACCESS]]
// SILGEN:   end_access [[ACCESS]]

func make_int() -> Int32 { return 0 }
func take_int_inout(_ value: inout Int32) {}

// CHECK-LABEL: sil hidden @$s10addressors6test_dys5Int32VAA1DVzF : $@convention(thin) (@inout D) -> Int32
// CHECK: bb0([[ARRAY:%.*]] : $*D):
func test_d(_ array: inout D) -> Int32 {
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors8make_ints5Int32VyF
// CHECK:   [[V:%.*]] = apply [[T0]]()
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [static] [[ARRAY]] : $*D
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors1DVys5Int32VAEciau
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[WRITE]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*Int32
// CHECK:   store [[V]] to [[ACCESS]] : $*Int32
  array[0] = make_int()

// CHECK:   [[WRITE:%.*]] = begin_access [modify] [static] [[ARRAY]] : $*D
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors1DVys5Int32VAEciau
// CHECK:   [[T1:%.*]] = apply [[T0]]({{%.*}}, [[WRITE]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]] : $UnsafeMutablePointer<Int32>,
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*Int32
// CHECK:   [[FN:%.*]] = function_ref @$s10addressors14take_int_inoutyys5Int32VzF
// CHECK:   apply [[FN]]([[ACCESS]])
  take_int_inout(&array[1])

// CHECK:   [[READ:%.*]] = begin_access [read] [static] [[ARRAY]] : $*D
// CHECK:   [[T0:%.*]] = load [[READ]]
// CHECK:   [[T1:%.*]] = function_ref @$s10addressors1DVys5Int32VAEcig
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

// CHECK-LABEL: sil hidden @$s10addressors6test_eyyAA1EVF
// CHECK: bb0([[E:%.*]] : $E):
// CHECK:   [[T0:%.*]] = function_ref @$s10addressors1EV5values5Int32Vvau
// CHECK:   [[T1:%.*]] = apply [[T0]]([[E]])
// CHECK:   [[T2:%.*]] = struct_extract [[T1]]
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]]
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[T3]] : $*Int32
// CHECK:   store {{%.*}} to [[ACCESS]] : $*Int32
func test_e(_ e: E) {
  e.value = 0
}

class Base {
  var data: UnsafeMutablePointer<Int32> = UnsafeMutablePointer.allocate(capacity: 100)

  var value: Int32 {
    unsafeAddress {
      return UnsafePointer(data)
    }
    unsafeMutableAddress {
      return data
    }
  }
}

class Sub : Base {
  override var value: Int32 {
    unsafeAddress {
      return UnsafePointer(data)
    }
    unsafeMutableAddress {
      return data
    }
  }
}

// rdar://problem/45046969
struct Bar<T> { }

protocol FooProtocol {
  subscript<T>(index: Bar<T>) -> T { get set }
}

// Make sure we get the right generic signatures for the synthesized
// getter/setter.

// CHECK: sil hidden [transparent] @$s10addressors3FooVyqd__AA3BarVyqd__Gcluig : $@convention(method) <Base><T> (Bar<T>, Foo<Base>)
// CHECK: sil hidden [transparent] @$s10addressors3FooVyqd__AA3BarVyqd__Gcluis : $@convention(method) <Base><T> (@in T, Bar<T>, @inout Foo<Base>) -> ()
struct Foo<Base>: FooProtocol {
  subscript<T>(index: Bar<T>) -> T {
    unsafeAddress {
      return UnsafePointer<T>(bitPattern: 0)!
    }
    unsafeMutableAddress {
      return UnsafeMutablePointer<T>(bitPattern: 0)!
    }
  }
}

// Make sure addressors don't get vtable entries.
// CHECK-LABEL: sil_vtable Base {
// CHECK-NEXT: #Base.data!getter.1: (Base) -> () -> UnsafeMutablePointer<Int32> : @$s10addressors4BaseC4dataSpys5Int32VGvg
// CHECK-NEXT: #Base.data!setter.1: (Base) -> (UnsafeMutablePointer<Int32>) -> () : @$s10addressors4BaseC4dataSpys5Int32VGvs
// CHECK-NEXT: #Base.data!modify.1: (Base) -> () -> () : @$s10addressors4BaseC4dataSpys5Int32VGvM
// CHECK-NEXT: #Base.value!getter.1: (Base) -> () -> Int32 : @$s10addressors4BaseC5values5Int32Vvg
// CHECK-NEXT: #Base.value!setter.1: (Base) -> (Int32) -> () : @$s10addressors4BaseC5values5Int32Vvs
// CHECK-NEXT: #Base.value!modify.1: (Base) -> () -> () : @$s10addressors4BaseC5values5Int32VvM
// CHECK-NEXT: #Base.init!allocator.1: (Base.Type) -> () -> Base : @$s10addressors4BaseCACycfC
// CHECK-NEXT: #Base.deinit!deallocator.1: @$s10addressors4BaseCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT: #Base.data!getter.1: (Base) -> () -> UnsafeMutablePointer<Int32> : @$s10addressors4BaseC4dataSpys5Int32VGvg
// CHECK-NEXT: #Base.data!setter.1: (Base) -> (UnsafeMutablePointer<Int32>) -> () : @$s10addressors4BaseC4dataSpys5Int32VGvs
// CHECK-NEXT: #Base.data!modify.1: (Base) -> () -> () : @$s10addressors4BaseC4dataSpys5Int32VGvM
// CHECK-NEXT: #Base.value!getter.1: (Base) -> () -> Int32 : @$s10addressors3SubC5values5Int32Vvg
// CHECK-NEXT: #Base.value!setter.1: (Base) -> (Int32) -> () : @$s10addressors3SubC5values5Int32Vvs
// CHECK-NEXT: #Base.value!modify.1: (Base) -> () -> () : @$s10addressors3SubC5values5Int32VvM
// CHECK-NEXT: #Base.init!allocator.1: (Base.Type) -> () -> Base : @$s10addressors3SubCACycfC
// CHECK-NEXT: #Sub.deinit!deallocator.1: @$s10addressors3SubCfD
// CHECK-NEXT: }

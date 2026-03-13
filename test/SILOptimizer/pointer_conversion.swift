// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -Xllvm -sil-print-types -emit-sil -O %s | %FileCheck %s
// REQUIRES: optimized_stdlib,swift_stdlib_no_asserts
// REQUIRES: swift_in_compiler

import Swift

// Opaque, unoptimizable functions to call.
@_silgen_name("takesConstRawPointer")
func takesConstRawPointer(_ x: UnsafeRawPointer)
@_silgen_name("takesOptConstRawPointer")
func takesOptConstRawPointer(_ x: UnsafeRawPointer?)
@_silgen_name("takesMutableRawPointer")
func takesMutableRawPointer(_ x: UnsafeMutableRawPointer)
@_silgen_name("takesOptMutableRawPointer")
func takesOptMutableRawPointer(_ x: UnsafeMutableRawPointer?)

// Opaque function for generating values
@_silgen_name("get")
func get<T>() -> T

// The purpose of these tests is to make sure the storage is never released
// before the call to the opaque function.

// CHECK-LABEL: sil @$s18pointer_conversion9testArrayyyF
public func testArray() {
  let array: [Int] = get()
  takesConstRawPointer(array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on {{.*}} : $__ContiguousArrayStorageBase
  // CHECK: [[FN:%.+]] = function_ref @takesConstRawPointer
  // CHECK-NEXT: apply [[FN]]([[DEP_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|__ContiguousArrayStorageBase}}
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$s18pointer_conversion19testArrayToOptionalyyF
public func testArrayToOptional() {
  let array: [Int] = get()
  takesOptConstRawPointer(array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on {{.*}} : $__ContiguousArrayStorageBase
  // CHECK-NEXT: [[OPT_POINTER:%.+]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt, [[DEP_POINTER]]
  // CHECK: [[FN:%.+]] = function_ref @takesOptConstRawPointer
  // CHECK-NEXT: apply [[FN]]([[OPT_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|__ContiguousArrayStorageBase}}
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$s18pointer_conversion16testMutableArrayyyF
public func testMutableArray() {
  var array: [Int] = get()
  takesMutableRawPointer(&array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeMutableRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on {{.*}} : $__ContiguousArrayStorageBase
  // CHECK: [[FN:%.+]] = function_ref @takesMutableRawPointer
  // CHECK: apply [[FN]]([[DEP_POINTER]])
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|__ContiguousArrayStorageBase}}
  // CHECK: dealloc_stack {{%.+}} : $*Array<Int>
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$s18pointer_conversion26testMutableArrayToOptionalyyF
public func testMutableArrayToOptional() {
  var array: [Int] = get()
  takesOptMutableRawPointer(&array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeMutableRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on {{.*}} : $__ContiguousArrayStorageBase
  // CHECK-NEXT: [[OPT_POINTER:%.+]] = enum $Optional<UnsafeMutableRawPointer>, #Optional.some!enumelt, [[DEP_POINTER]]
  // CHECK: [[FN:%.+]] = function_ref @takesOptMutableRawPointer
  // CHECK: apply [[FN]]([[OPT_POINTER]])
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|__ContiguousArrayStorageBase}}
  // CHECK: dealloc_stack {{%.+}} : $*Array<Int>
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$s18pointer_conversion21arrayLiteralPromotionyyF
public func arrayLiteralPromotion() {
  takesConstRawPointer([-41,-42,-43,-44])
  
  // Outline the array literal.
  // CHECK: [[ARR:%.+]] = global_value
  // CHECK: [[CAST:%.+]] = upcast [[ARR]]
  // CHECK: [[TADDR:%.+]] = ref_tail_addr [[CAST]]
  // CHECK: [[RAWPTR:%.+]] = address_to_pointer [[TADDR]]
  // CHECK: [[UNSAFEPTR:%.+]] = struct $UnsafeRawPointer ([[RAWPTR]]
  // CHECK: [[PTR:%.+]] = mark_dependence [[UNSAFEPTR]]
  // CHECK: [[FN:%.+]] = function_ref @takesConstRawPointer
  // CHECK: apply [[FN]]([[PTR]])
}

// Test sil verification at -O with bind_memory and rebind_memory
// where the rebind is in a defer block.
//
// CHECK-LABEL: sil @$s18pointer_conversion21testWithMemoryRebound6rawPtr2to8capacity_q_Bp_xmSiq_SPyxGKXEtKr0_lF
// CHECK: [[BIND:%.*]] = bind_memory %1 : $Builtin.RawPointer, %{{.*}} : $Builtin.Word to $*T
// CHECK: rebind_memory %1 : $Builtin.RawPointer to [[BIND]] : $Builtin.Word
// CHECK-LABEL: } // end sil function '$s18pointer_conversion21testWithMemoryRebound6rawPtr2to8capacity_q_Bp_xmSiq_SPyxGKXEtKr0_lF'
public func testWithMemoryRebound<T, Result>(
  rawPtr: Builtin.RawPointer,
  to type: T.Type,
  capacity count: Int,
  _ body: (_ pointer: UnsafePointer<T>) throws -> Result
) rethrows -> Result {
  let binding =
    Builtin.bindMemory(rawPtr, count._builtinWordValue, T.self)
  defer {
    Builtin.rebindMemory(rawPtr, binding)
  }
  return try body(.init(rawPtr))
}

@_silgen_name("takeRawPointer")
func takeRawPointer(_: UnsafeRawPointer)

@_silgen_name("takeObjectPointer")
func takeObjectPointer(_: UnsafePointer<AnyObject>)

@_silgen_name("takeStringPointer")
func takeStringPointer(_: UnsafePointer<String>)

@_silgen_name("takeDictionaryPointer")
func takeDictionaryPointer(_: UnsafePointer<Dictionary<Int, Int>>)

// Test conversion of a dictionary to a raw pointer. This is not a sound
// conversion, but the compiler must still generate memory-safe code.
//
// A dictionary is an eager-move type, so it will be destroyed at its
// last use. An address_to_pointer operation escapes the pointer, so
// the compiler never sees those pointer uses. The pointer's scope
// must be protected by a fix_lifetime.
//
// NOTE: If this test triggers a compiler error because of the unsafe
// inout conversion, then we have arrived at a better world. Delete
// the test. The eagerMoveToPointer test below is sufficient.
//
// CHECK-LABEL: sil [stack_protection] @$s18pointer_conversion22dictionaryToRawPointeryyF : $@convention(thin) () -> () {
// CHECK: [[A:%.*]] = alloc_stack [var_decl] $Dictionary<Int, Int>, var, name "d"
// CHECK: [[PTR:%.*]] = address_to_pointer [stack_protection] [[A]] : $*Dictionary<Int, Int> to $Builtin.RawPointer
// CHECK: [[UP:%.*]] = struct $UnsafeRawPointer ([[PTR]] : $Builtin.RawPointer)
// CHECK: apply %{{.*}}([[UP]]) : $@convention(thin) (UnsafeRawPointer) -> ()
// CHECK: [[D:%.*]] = load %0 : $*Dictionary<Int, Int>
// CHECK: fix_lifetime [[D]] : $Dictionary<Int, Int>
// CHECK: release_value [[D]] : $Dictionary<Int, Int>
// CHECK: dealloc_stack [[A]] : $*Dictionary<Int, Int>
// CHECK-LABEL: } // end sil function '$s18pointer_conversion22dictionaryToRawPointeryyF'
public func dictionaryToRawPointer() {
  var d = [1:1]
  takeRawPointer(&d)
}

// Test conversion of a non-trivial eager-move type to a raw pointer.
// This currently only applies to Dictionary, but converting a
// dictionary to a pointer will likely be a compiler error in the
// future. So force an eagerMove type here.
//
// An eager-move type will be destroyed at its last use. An
// address_to_pointer operation escapes the pointer, so the compiler
// never sees those pointer uses. The pointer's scope must be
// protected by a fix_lifetime.
// CHECK-LABEL: sil [stack_protection] @$s18pointer_conversion18eagerMoveToPointer1oyyXln_tF : $@convention(thin) (@owned AnyObject) -> () {
// CHECK: [[A:%.*]] = alloc_stack [var_decl] [moveable_value_debuginfo] $AnyObject, var, name "o"
// CHECK: [[PTR:%.*]] = address_to_pointer [stack_protection] [[A]] : $*AnyObject to $Builtin.RawPointer
// CHECK: [[UP:%.*]] = struct $UnsafePointer<AnyObject> ([[PTR]] : $Builtin.RawPointer)
// CHECK: apply %{{.*}}([[UP]]) : $@convention(thin) (UnsafePointer<AnyObject>) -> ()
// CHECK: [[O:%.*]] = load [[A]] : $*AnyObject
// CHECK: fix_lifetime [[O]] : $AnyObject
// CHECK: strong_release [[O]] : $AnyObject
// CHECK: dealloc_stack [[A]] : $*AnyObject
// CHECK-LABEL: } // end sil function '$s18pointer_conversion18eagerMoveToPointer1oyyXln_tF'
public func eagerMoveToPointer(@_eagerMove o: consuming AnyObject ) {
  takeObjectPointer(&o)
}

// CHECK-LABEL: sil [stack_protection] @$s18pointer_conversion15stringToPointer2ssySS_tF : $@convention(thin) (@guaranteed String) -> () {
// CHECK:   [[A:%.*]] = alloc_stack [var_decl] $String, var, name "s"
// CHECK:   [[PTR:%.*]] = address_to_pointer [stack_protection] [[A]] : $*String to $Builtin.RawPointer
// CHECK:   [[UP:%.*]] = struct $UnsafePointer<String> ([[PTR]] : $Builtin.RawPointer)
// CHECK:   apply {{.*}}([[UP]]) : $@convention(thin) (UnsafePointer<String>) -> ()
// CHECK:   [[S:%.*]] = load [[A]] : $*String
// CHECK:   fix_lifetime [[S]] : $String
// CHECK:   release_value [[S]] : $String
// CHECK:   dealloc_stack [[A]] : $*String
// CHECK-LABEL: } // end sil function '$s18pointer_conversion15stringToPointer2ssySS_tF'
public func stringToPointer(ss: String) {
  var s = ss
  takeStringPointer(&s)
}

// CHECK-LABEL: sil [stack_protection] @$s18pointer_conversion19dictionaryToPointer2ddySDyS2iG_tF : $@convention(thin) (@guaranteed Dictionary<Int, Int>) -> () {
// CHECK:   [[A:%.*]] = alloc_stack [var_decl] $Dictionary<Int, Int>, var, name "d"
// CHECK:   [[PTR:%.*]] = address_to_pointer [stack_protection] [[A]] : $*Dictionary<Int, Int> to $Builtin.RawPointer
// CHECK:   [[UP:%.*]] = struct $UnsafePointer<Dictionary<Int, Int>> ([[PTR]] : $Builtin.RawPointer)
// CHECK:   apply %{{.*}}([[UP]]) : $@convention(thin) (UnsafePointer<Dictionary<Int, Int>>) -> ()
// CHECK:   [[D:%.*]] = load [[A]] : $*Dictionary<Int, Int>
// CHECK:   fix_lifetime [[D]] : $Dictionary<Int, Int>
// CHECK:   release_value [[D]] : $Dictionary<Int, Int>
// CHECK:   dealloc_stack [[A]] : $*Dictionary<Int, Int>
// CHECK-LABEL: } // end sil function '$s18pointer_conversion19dictionaryToPointer2ddySDyS2iG_tF'
public func dictionaryToPointer(dd: Dictionary<Int, Int>) {
  var d = dd
  takeDictionaryPointer(&d)
}

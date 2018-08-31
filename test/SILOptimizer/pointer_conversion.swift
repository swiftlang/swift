// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s
// REQUIRES: optimized_stdlib

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

// CHECK-LABEL: sil @$S18pointer_conversion9testArrayyyF
public func testArray() {
  let array: [Int] = get()
  takesConstRawPointer(array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on {{.*}} : $_ContiguousArrayStorageBase
  // CHECK: [[FN:%.+]] = function_ref @takesConstRawPointer
  // CHECK-NEXT: apply [[FN]]([[DEP_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|_ContiguousArrayStorageBase}}
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$S18pointer_conversion19testArrayToOptionalyyF
public func testArrayToOptional() {
  let array: [Int] = get()
  takesOptConstRawPointer(array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on {{.*}} : $_ContiguousArrayStorageBase
  // CHECK-NEXT: [[OPT_POINTER:%.+]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt.1, [[DEP_POINTER]]
  // CHECK: [[FN:%.+]] = function_ref @takesOptConstRawPointer
  // CHECK-NEXT: apply [[FN]]([[OPT_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|_ContiguousArrayStorageBase}}
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$S18pointer_conversion16testMutableArrayyyF
public func testMutableArray() {
  var array: [Int] = get()
  takesMutableRawPointer(&array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeMutableRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on {{.*}} : $_ContiguousArrayStorageBase
  // CHECK: [[FN:%.+]] = function_ref @takesMutableRawPointer
  // CHECK-NEXT: apply [[FN]]([[DEP_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|_ContiguousArrayStorageBase}}
  // CHECK-NEXT: dealloc_stack {{%.+}} : $*Array<Int>
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$S18pointer_conversion26testMutableArrayToOptionalyyF
public func testMutableArrayToOptional() {
  var array: [Int] = get()
  takesOptMutableRawPointer(&array)
  // CHECK: [[POINTER:%.+]] = struct $UnsafeMutableRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on {{.*}} : $_ContiguousArrayStorageBase
  // CHECK-NEXT: [[OPT_POINTER:%.+]] = enum $Optional<UnsafeMutableRawPointer>, #Optional.some!enumelt.1, [[DEP_POINTER]]
  // CHECK: [[FN:%.+]] = function_ref @takesOptMutableRawPointer
  // CHECK-NEXT: apply [[FN]]([[OPT_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: strong_release {{%.+}} : ${{Builtin[.]BridgeObject|_ContiguousArrayStorageBase}}
  // CHECK-NEXT: dealloc_stack {{%.+}} : $*Array<Int>
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]
}

// CHECK-LABEL: sil @$S18pointer_conversion21arrayLiteralPromotionyyF
public func arrayLiteralPromotion() {
  takesConstRawPointer([41,42,43,44])
  
  // Stack allocate the array.
  // TODO: When stdlib checks are enabled, this becomes heap allocated... :-(
  // CHECK: alloc_ref {{.*}}[tail_elems $Int * {{.*}} : $Builtin.Word] $_ContiguousArrayStorage<Int>
  
  // Store the elements.
  // CHECK: [[ELT:%.+]] = integer_literal $Builtin.Int{{.*}}, 41
  // CHECK: [[ELT:%.+]] = integer_literal $Builtin.Int{{.*}}, 42
  // CHECK: [[ELT:%.+]] = integer_literal $Builtin.Int{{.*}}, 43
  // CHECK: [[ELT:%.+]] = integer_literal $Builtin.Int{{.*}}, 44
  
  // Call the function.
  // CHECK: [[PTR:%.+]] = mark_dependence

  // CHECK: [[FN:%.+]] = function_ref @takesConstRawPointer
  // CHECK: apply [[FN]]([[PTR]])
  
  // Release the heap value.
  // CHECK: strong_release
}


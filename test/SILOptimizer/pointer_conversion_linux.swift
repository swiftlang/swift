// RUN: %target-swift-frontend -module-name pointer_conversion -emit-sil -O %s | %FileCheck %s
// REQUIRES: optimized_stdlib
// UNSUPPORTED: objc_interop

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

// CHECK-LABEL: sil @$s18pointer_conversion17testOptionalArrayyyF
public func testOptionalArray() {
  let array: [Int]? = get()
  takesOptConstRawPointer(array)
  // CHECK: bb0:
  // CHECK:   switch_enum {{.*}}, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

  // CHECK: [[SOME_BB]](
  // CHECK: [[OWNER:%.+]] = enum $Optional<AnyObject>, #Optional.some!enumelt.1,
  // CHECK-NEXT: [[POINTER:%.+]] = struct $UnsafeRawPointer (
  // CHECK-NEXT: [[DEP_POINTER:%.+]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]] : $Optional<AnyObject>
  // CHECK-NEXT: [[OPT_POINTER:%.+]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt.1, [[DEP_POINTER]]
  // CHECK-NEXT: br [[CALL_BRANCH:bb[0-9]+]]([[OPT_POINTER]] : $Optional<UnsafeRawPointer>, [[OWNER]] : $Optional<AnyObject>)

  // CHECK: [[CALL_BRANCH]]([[OPT_POINTER:%.+]] : $Optional<UnsafeRawPointer>, [[OWNER:%.+]] : $Optional<AnyObject>):
  // CHECK-NOT: release
  // CHECK-NEXT: [[DEP_OPT_POINTER:%.+]] = mark_dependence [[OPT_POINTER]] : $Optional<UnsafeRawPointer> on [[OWNER]] : $Optional<AnyObject>
  // CHECK: [[FN:%.+]] = function_ref @takesOptConstRawPointer
  // CHECK-NEXT: apply [[FN]]([[DEP_OPT_POINTER]])
  // CHECK-NOT: release
  // CHECK-NOT: {{^bb[0-9]+:}}
  // CHECK: release_value {{%.+}} : $Optional<Array<Int>>
  // CHECK-NEXT: [[EMPTY:%.+]] = tuple ()
  // CHECK-NEXT: return [[EMPTY]]

  // CHECK: [[NONE_BB]]:
  // CHECK-NEXT: [[NO_POINTER:%.+]] = enum $Optional<UnsafeRawPointer>, #Optional.none!enumelt
  // CHECK-NEXT: [[NO_OWNER:%.+]] = enum $Optional<AnyObject>, #Optional.none!enumelt
  // CHECK-NEXT: br [[CALL_BRANCH]]([[NO_POINTER]] : $Optional<UnsafeRawPointer>, [[NO_OWNER]] : $Optional<AnyObject>)

} // CHECK: end sil function '$s18pointer_conversion17testOptionalArrayyyF'

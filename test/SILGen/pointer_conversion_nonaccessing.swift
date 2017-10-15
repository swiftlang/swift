// RUN: %target-swift-frontend -swift-version 4 -enable-sil-ownership -emit-silgen %s | %FileCheck %s

// rdar://33265254

// Check for the total absence of access markers here.
// FIXME: probably we should have some markers that just disable even static checking

var global = 0

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing6testEqSbSV3ptr_tF
func testEq(ptr: UnsafeRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing7testNeqSbSV3ptr_tF
func testNeq(ptr: UnsafeRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing6testEqSbSv3ptr_tF 
func testEq(ptr: UnsafeMutableRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing7testNeqSbSv3ptr_tF
func testNeq(ptr: UnsafeMutableRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing6testEqSbSPySiG3ptr_tF
func testEq(ptr: UnsafePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing7testNeqSbSPySiG3ptr_tF
func testNeq(ptr: UnsafePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing6testEqSbSpySiG3ptr_tF
func testEq(ptr: UnsafeMutablePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @_T031pointer_conversion_nonaccessing7testNeqSbSpySiG3ptr_tF
func testNeq(ptr: UnsafeMutablePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @_T031pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// RUN: %target-swift-emit-silgen -swift-version 4 -enable-sil-ownership %s | %FileCheck %s

// rdar://33265254

// Check for the total absence of access markers here.
// FIXME: probably we should have some markers that just disable even static checking

var global = 0

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing6testEq3ptrSbSV_tF
func testEq(ptr: UnsafeRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing7testNeq3ptrSbSV_tF
func testNeq(ptr: UnsafeRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing6testEq3ptrSbSv_tF
func testEq(ptr: UnsafeMutableRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing7testNeq3ptrSbSv_tF
func testNeq(ptr: UnsafeMutableRawPointer) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing6testEq3ptrSbSPySiG_tF
func testEq(ptr: UnsafePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing7testNeq3ptrSbSPySiG_tF
func testNeq(ptr: UnsafePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing6testEq3ptrSbSpySiG_tF
func testEq(ptr: UnsafeMutablePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global == ptr
}

// CHECK-LABEL: sil hidden @$s31pointer_conversion_nonaccessing7testNeq3ptrSbSpySiG_tF
func testNeq(ptr: UnsafeMutablePointer<Int>) -> Bool {
  // CHECK: [[T0:%.*]] = global_addr @$s31pointer_conversion_nonaccessing6globalSiv
  // CHECK: address_to_pointer [[T0]]
  return &global != ptr
}

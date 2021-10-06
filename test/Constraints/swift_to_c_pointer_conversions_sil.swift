// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -import-objc-header %S/Inputs/c_pointer_conversions.h %s -emit-sil -verify | %FileCheck %s

// REQUIRES: objc_interop


// Check that implicit conversions don't make expressions ambiguous
func test_overloaded_ref_is_not_ambiguous() {
  func overloaded_func() -> UnsafeRawPointer { fatalError() }
  func overloaded_func() -> UnsafePointer<Int8> { fatalError() }

  func overloaded_mutable_func() -> UnsafeMutableRawPointer { fatalError() }
  func overloaded_mutable_func() -> UnsafeMutablePointer<Int8> { fatalError() }

  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G5_funcL0_SPys4Int8VGyF
  // CHECK-NEXT: apply
  const_char_ptr_func(overloaded_func()) // Ok
  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G5_funcL0_SPys4Int8VGyF
  // CHECK-NEXT: apply
  const_opt_char_ptr_func(overloaded_func()) // Ok

  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G13_mutable_funcL0_Spys4Int8VGyF
  // CHECK-NEXT: apply
  char_ptr_func(overloaded_mutable_func()) // Ok

  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G13_mutable_funcL0_Spys4Int8VGyF
  // CHECK-NEXT: apply
  const_char_ptr_func(overloaded_mutable_func()) // Ok (picks UnsafeMutablePointer using implicit conversion)

  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G13_mutable_funcL0_Spys4Int8VGyF
  // CHECK-NEXT: apply
  opt_char_ptr_func(overloaded_mutable_func()) // Ok

  // CHECK: function_ref @$s34swift_to_c_pointer_conversions_sil36test_overloaded_ref_is_not_ambiguousyyF0G13_mutable_funcL0_Spys4Int8VGyF
  // CHECK-NEXT: apply
  const_opt_char_ptr_func(overloaded_mutable_func()) // Ok
}

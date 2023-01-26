// RUN: %swift-frontend %s -enable-import-ptrauth-field-function-pointers -emit-silgen -target arm64e-apple-ios13.0 -I %S/Inputs/ 2>&1 | %FileCheck %s
// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

import PointerAuth

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import05test_B8_fn_reads5Int32VyF :
// CHECK: [[GLOB:%.*]] = global_addr @ptr_to_secure_struct : $*Optional<UnsafeMutablePointer<SecureStruct>>
// CHECK: [[A1:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*Optional<UnsafeMutablePointer<SecureStruct>>
// CHECK: [[LD1:%.*]] = load [trivial] [[A1]] : $*Optional<UnsafeMutablePointer<SecureStruct>>
// CHECK: end_access [[A1]] : $*Optional<UnsafeMutablePointer<SecureStruct>>
// CHECK: switch_enum [[LD1]] : $Optional<UnsafeMutablePointer<SecureStruct>>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb2([[BBARG1:%.*]] : $UnsafeMutablePointer<SecureStruct>):
// CHECK: [[F1:%.*]] = function_ref @$sSp7pointeexvlu : $@convention(method) <τ_0_0> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[PTR1:%.*]] = apply [[F1]]<SecureStruct>([[BBARG1]]) : $@convention(method) <τ_0_0> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[RAW1:%.*]] = struct_extract [[PTR1]] : $UnsafePointer<SecureStruct>, #UnsafePointer._rawValue
// CHECK: [[ADDR1:%.*]] = pointer_to_address [[RAW1]] : $Builtin.RawPointer to [strict] $*SecureStruct
// CHECK: [[A2:%.*]] = begin_access [read] [unsafe] [[ADDR1]] : $*SecureStruct
// CHECK: [[ELEM1:%.*]] = struct_element_addr [[A2]] : $*SecureStruct, #SecureStruct.secure_func_ptr1
// CHECK: [[ASIGNED:%.*]] = begin_access [read] [signed] [[ELEM1]] : $*Optional<@convention(c) () -> Int32>
// CHECK: [[FPLOAD:%.*]] = load [trivial] [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: end_access [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: end_access [[A2]] : $*SecureStruct
// CHECK: switch_enum [[FPLOAD]] : $Optional<@convention(c) () -> Int32>, case #Optional.some!enumelt: bb4, case #Optional.none!enumelt: bb3
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import05test_B8_fn_reads5Int32VyF'
func test_field_fn_read() -> Int32 {
  let fn = ptr_to_secure_struct!.pointee.secure_func_ptr1!
  return fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import05test_B12_fn_ptr_swapyyF :
// CHECK:bb8([[UMP:%.*]] : $UnsafeMutablePointer<SecureStruct>):
// CHECK:  [[F1:%.*]] = function_ref @$sSp7pointeexvau : $@convention(method) <τ_0_0> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[P:%.*]] = apply [[F1]]<SecureStruct>([[UMP]]) : $@convention(method) <τ_0_0> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[EX1:%.*]] = struct_extract [[P]] : $UnsafeMutablePointer<SecureStruct>, #UnsafeMutablePointer._rawValue
// CHECK:  [[ADDR:%.*]] = pointer_to_address [[EX1]] : $Builtin.RawPointer to [strict] $*SecureStruct
// CHECK:  [[A1:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*SecureStruct
// CHECK:  [[ELEM1:%.*]] = struct_element_addr [[A1]] : $*SecureStruct, #SecureStruct.secure_func_ptr2
// CHECK:  [[A2:%.*]] = begin_access [modify] [signed] [[ELEM1]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  assign {{.*}} to [[A2]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  end_access [[A2]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  end_access [[A1]] : $*SecureStruct
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import05test_B12_fn_ptr_swapyyF'
func test_field_fn_ptr_swap() {
  let t = ptr_to_secure_struct!.pointee.secure_func_ptr1
  ptr_to_secure_struct!.pointee.secure_func_ptr1 = ptr_to_secure_struct!.pointee.secure_func_ptr2
  ptr_to_secure_struct!.pointee.secure_func_ptr2 = t
}


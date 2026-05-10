// RUN: %swift-frontend %s -emit-silgen -Xllvm -sil-print-types -target arm64e-apple-ios13.0 -I %S/Inputs/ | %FileCheck %s

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
// CHECK: [[F1:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[PTR1:%.*]] = apply [[F1]]<SecureStruct>([[BBARG1]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[RAW1:%.*]] = struct_extract [[PTR1]] : $UnsafePointer<SecureStruct>, #UnsafePointer._rawValue
// CHECK: [[ADDR1:%.*]] = pointer_to_address [[RAW1]] : $Builtin.RawPointer to [strict] $*SecureStruct
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[ADDR1]] : $*SecureStruct on [[BBARG1]] : $UnsafeMutablePointer<SecureStruct>
// CHECK: [[A2:%.*]] = begin_access [read] [unsafe] [[MD]] : $*SecureStruct
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
// CHECK:  [[F1:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[P:%.*]] = apply [[F1]]<SecureStruct>([[UMP]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[EX1:%.*]] = struct_extract [[P]] : $UnsafeMutablePointer<SecureStruct>, #UnsafeMutablePointer._rawValue
// CHECK:  [[ADDR:%.*]] = pointer_to_address [[EX1]] : $Builtin.RawPointer to [strict] $*SecureStruct
// CHECK:  [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*SecureStruct on [[UMP]] : $UnsafeMutablePointer<SecureStruct>
// CHECK:  [[A1:%.*]] = begin_access [modify] [unsafe] [[MD]] : $*SecureStruct
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

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import05test_B12_fn_ptr_temps5Int32VyF :
// CHECK:   [[STK:%.*]] = alloc_stack [var_decl] $SecureStruct, let, name "struct_with_signed_val"
// CHECK:   [[FLD:%.*]] = struct_element_addr [[STK]] : $*SecureStruct, #SecureStruct.secure_func_ptr1
// CHECK:   [[SIGNED:%.*]] = begin_access [read] [signed] [[FLD]] : $*Optional<@convention(c) () -> Int32>
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import05test_B12_fn_ptr_temps5Int32VyF'
func test_field_fn_ptr_temp() -> Int32 {
  let struct_with_signed_val = ptr_to_secure_struct.pointee
  return struct_with_signed_val.secure_func_ptr1()
}

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import024test_addr_discriminated_B8_fn_reads5Int32VyF :
// CHECK: [[GLOB:%.*]] = global_addr @ptr_to_addr_discriminated_secure_struct : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: [[A1:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: [[LD1:%.*]] = load [trivial] [[A1]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: end_access [[A1]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: switch_enum [[LD1]] : $Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb2([[BBARG1:%.*]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>):
// CHECK: [[F1:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[PTR1:%.*]] = apply [[F1]]<AddressDiscriminatedSecureStruct>([[BBARG1]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[RAW1:%.*]] = struct_extract [[PTR1]] : $UnsafePointer<AddressDiscriminatedSecureStruct>, #UnsafePointer._rawValue
// CHECK: [[ADDR1:%.*]] = pointer_to_address [[RAW1]] : $Builtin.RawPointer to [strict] $*AddressDiscriminatedSecureStruct
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[ADDR1]] : $*AddressDiscriminatedSecureStruct on [[BBARG1]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct> // user: %20
// CHECK: [[A2:%.*]] = begin_access [read] [unsafe] [[MD]] : $*AddressDiscriminatedSecureStruct
// CHECK: [[ELEM1:%.*]] = struct_element_addr [[A2]] : $*AddressDiscriminatedSecureStruct, #AddressDiscriminatedSecureStruct.secure_func_ptr1
// CHECK: [[ASIGNED:%.*]] = begin_access [read] [signed] [[ELEM1]] : $*Optional<@convention(c) () -> Int32>
// CHECK: [[FPLOAD:%.*]] = load [trivial] [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: end_access [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: end_access [[A2]] : $*AddressDiscriminatedSecureStruct
// CHECK: switch_enum [[FPLOAD]] : $Optional<@convention(c) () -> Int32>, case #Optional.some!enumelt: bb4, case #Optional.none!enumelt: bb3
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import024test_addr_discriminated_B8_fn_reads5Int32VyF'
func test_addr_discriminated_field_fn_read() -> Int32 {
  let fn = ptr_to_addr_discriminated_secure_struct!.pointee.secure_func_ptr1!
  return fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import024test_addr_discriminated_B12_fn_ptr_swapyyF :
// CHECK:bb8([[UMP:%.*]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>):
// CHECK:  [[F1:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[P:%.*]] = apply [[F1]]<AddressDiscriminatedSecureStruct>([[UMP]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:  [[EX1:%.*]] = struct_extract [[P]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>, #UnsafeMutablePointer._rawValue
// CHECK:  [[ADDR:%.*]] = pointer_to_address [[EX1]] : $Builtin.RawPointer to [strict] $*AddressDiscriminatedSecureStruct
// CHECK:  [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*AddressDiscriminatedSecureStruct on [[UMP]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>
// CHECK:  [[A1:%.*]] = begin_access [modify] [unsafe] [[MD]] : $*AddressDiscriminatedSecureStruct
// CHECK:  [[ELEM1:%.*]] = struct_element_addr [[A1]] : $*AddressDiscriminatedSecureStruct, #AddressDiscriminatedSecureStruct.secure_func_ptr2
// CHECK:  [[A2:%.*]] = begin_access [modify] [signed] [[ELEM1]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  assign {{.*}} to [[A2]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  end_access [[A2]] : $*Optional<@convention(c) () -> Int32>
// CHECK:  end_access [[A1]] : $*AddressDiscriminatedSecureStruct
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import024test_addr_discriminated_B12_fn_ptr_swapyyF'
func test_addr_discriminated_field_fn_ptr_swap() {
  let t = ptr_to_addr_discriminated_secure_struct!.pointee.secure_func_ptr1
  ptr_to_addr_discriminated_secure_struct!.pointee.secure_func_ptr1 = ptr_to_addr_discriminated_secure_struct!.pointee.secure_func_ptr2
  ptr_to_addr_discriminated_secure_struct!.pointee.secure_func_ptr2 = t
}

// CHECK-LABEL: sil hidden [ossa] @$s25ptrauth_field_fptr_import21test_addr_only_structs5Int32VyF :
// CHECK: [[GLOB:%.*]] = global_addr @ptr_to_addr_discriminated_secure_struct : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: [[STK:%.*]] = alloc_stack [var_decl] $AddressDiscriminatedSecureStruct, let, name "struct_with_signed_val"
// CHECK: [[A1:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: [[LD1:%.*]] = load [trivial] [[A1]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: end_access [[A1]] : $*Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>
// CHECK: switch_enum [[LD1]] : $Optional<UnsafeMutablePointer<AddressDiscriminatedSecureStruct>>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb2([[BBARG1:%.*]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>):
// CHECK: [[F1:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[PTR1:%.*]] = apply [[F1]]<AddressDiscriminatedSecureStruct>([[BBARG1]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK: [[RAW1:%.*]] = struct_extract [[PTR1]] : $UnsafePointer<AddressDiscriminatedSecureStruct>, #UnsafePointer._rawValue
// CHECK: [[ADDR1:%.*]] = pointer_to_address [[RAW1]] : $Builtin.RawPointer to [strict] $*AddressDiscriminatedSecureStruct
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[ADDR1]] : $*AddressDiscriminatedSecureStruct on [[BBARG1]] : $UnsafeMutablePointer<AddressDiscriminatedSecureStruct>
// CHECK: [[A2:%.*]] = begin_access [read] [unsafe] [[MD]] : $*AddressDiscriminatedSecureStruct
// CHECK: copy_addr [[A2]] to [init] [[STK]] : $*AddressDiscriminatedSecureStruct
// CHECK: end_access [[A2]] : $*AddressDiscriminatedSecureStruct
// CHECK: [[ELEM1:%.*]] = struct_element_addr [[STK]] : $*AddressDiscriminatedSecureStruct, #AddressDiscriminatedSecureStruct.secure_func_ptr1
// CHECK: [[ASIGNED:%.*]] = begin_access [read] [signed] [[ELEM1]] : $*Optional<@convention(c) () -> Int32>
// CHECK: [[FPLOAD:%.*]] = load [trivial] [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: end_access [[ASIGNED]] : $*Optional<@convention(c) () -> Int32>
// CHECK: switch_enum [[FPLOAD]] : $Optional<@convention(c) () -> Int32>, case #Optional.some!enumelt: bb4, case #Optional.none!enumelt: bb3
// CHECK-LABEL: } // end sil function '$s25ptrauth_field_fptr_import21test_addr_only_structs5Int32VyF'
func test_addr_only_struct() -> Int32 {
  let struct_with_signed_val = ptr_to_addr_discriminated_secure_struct.pointee
  return struct_with_signed_val.secure_func_ptr1()
}


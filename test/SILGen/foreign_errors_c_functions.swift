// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -module-name foreign_errors_c -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import swift_error_c_functions

// --- nonnull_error: _Bool return, result discarded after error check ---
// Mirrors testNonNilError() in foreign_errors.swift (nonnull_error convention).

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c20testNonnullErrorBoolyyKF : $@convention(thin) () -> @error any Error
func testNonnullErrorBool() throws {
  //   Allocate error temp and initialize to nil.
  // CHECK: [[ERR_TEMP:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
  // CHECK: inject_enum_addr [[ERR_TEMP]] : $*Optional<NSError>, #Optional.none!enumelt

  //   Get the C function reference (not objc_method).
  // CHECK: [[FN:%.*]] = function_ref @$sSo13c_error_boundSbyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool

  //   Prepare the error pointer and call.
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool

  //   For nonnull_error, check the error pointer directly (no result comparison).
  // CHECK: switch_enum {{%.*}} : $Optional<NSError>, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]

  //   Normal: discard result.
  // CHECK: [[NORMAL_BB]]:
  // CHECK: ignored_use [[RESULT]] : $Bool
  try c_error_bound()
}

// --- nonnull_error: Float return ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c21testNonnullErrorFloatSfyKF : $@convention(thin) () -> (Float, @error any Error)
func testNonnullErrorFloat() throws -> Float {
  // CHECK: [[FN:%.*]] = function_ref @$sSo14c_error_bounceSfyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Float
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Float
  // CHECK: switch_enum {{%.*}} : $Optional<NSError>, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]
  // CHECK: [[NORMAL_BB]]:
  // CHECK-NOT: destroy_value
  // CHECK: return [[RESULT]] : $Float
  return try c_error_bounce()
}

// --- nonnull_error: Void return ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c20testNonnullErrorVoidyyKF : $@convention(thin) () -> @error any Error
func testNonnullErrorVoid() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo15c_error_flounceyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> ()
  // CHECK: apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> ()
  // CHECK: switch_enum {{%.*}} : $Optional<NSError>, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]
  try c_error_flounce()
}

// --- zero_result: int return → ZeroPreservedResult (result preserved) ---
// Mirrors testPreservedResult() in foreign_errors.swift for ObjC ounce().

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c19testPreservedResults5Int32VyKF : $@convention(thin) () -> (Int32, @error any Error)
func testPreservedResult() throws -> CInt {
  // CHECK: [[FN:%.*]] = function_ref @$sSo13c_error_ounces5Int32VyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Int32
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Int32

  //   Compare result against zero.
  // CHECK: [[T0:%.*]] = struct_extract [[RESULT]]
  // CHECK: [[ZERO:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
  // CHECK: [[CMP:%.*]] = builtin "cmp_ne_Int32"([[T0]] : $[[PRIM]], [[ZERO]] : $[[PRIM]])
  // CHECK: cond_br [[CMP]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]

  //   Normal: return preserved result.
  // CHECK: [[NORMAL_BB]]:
  // CHECK-NOT: destroy_value
  // CHECK: return [[RESULT]] : $Int32
  return try c_error_ounce()
}

// --- nonzero_result: int return → inverted branch ---
// Mirrors testPreservedResultInverted() in foreign_errors.swift for ObjC once().

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c27testPreservedResultInvertedyyKF : $@convention(thin) () -> @error any Error
func testPreservedResultInverted() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo12c_error_onceyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Int32
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Int32

  //   Compare result against zero — note INVERTED branch targets.
  // CHECK: [[T0:%.*]] = struct_extract [[RESULT]]
  // CHECK: [[ZERO:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
  // CHECK: [[CMP:%.*]] = builtin "cmp_ne_Int32"([[T0]] : $[[PRIM]], [[ZERO]] : $[[PRIM]])
  // CHECK: cond_br [[CMP]], [[ERROR_BB:bb[0-9]+]], [[NORMAL_BB:bb[0-9]+]]

  // CHECK: [[NORMAL_BB]]:
  // CHECK-NOT: destroy_value
  // CHECK: return {{%.+}} : $()
  try c_error_once()
}

// --- zero_result: _Bool return → ZeroResult (Void) ---
// Mirrors test0() in foreign_errors.swift for ObjC fail().

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c15testZeroResultByyKF : $@convention(thin) () -> @error any Error
func testZeroResultB() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo14c_error_sconceyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool

  //   Bool result: extract _value and branch directly.
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]

  // CHECK: [[NORMAL_BB]]:
  try c_error_sconce()
}

// --- nonzero_result: _Bool return → inverted branch ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c23testNonzeroResultScotchyyKF : $@convention(thin) () -> @error any Error
func testNonzeroResultScotch() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo14c_error_scotchyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool

  //   Bool result with inverted semantics.
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[ERROR_BB:bb[0-9]+]], [[NORMAL_BB:bb[0-9]+]]

  // CHECK: [[NORMAL_BB]]:
  try c_error_scotch()
}

// --- Error conversion: verify NSError → Error bridging on error path ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c20testErrorConversionByyKF : $@convention(thin) () -> @error any Error
func testErrorConversionB() throws {
  // CHECK: [[ERR_TEMP:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
  // CHECK: inject_enum_addr [[ERR_TEMP]] : $*Optional<NSError>, #Optional.none!enumelt

  // CHECK: [[FN:%.*]] = function_ref @$sSo14c_error_sconceyyKFTo

  // CHECK: [[UNMANAGED_TEMP:%.*]] = alloc_stack $@sil_unmanaged Optional<NSError>
  // CHECK: [[T0:%.*]] = load_borrow [[ERR_TEMP]]
  // CHECK: [[T1:%.*]] = ref_to_unmanaged [[T0]]
  // CHECK: store [[T1]] to [trivial] [[UNMANAGED_TEMP]]
  // CHECK: address_to_pointer [stack_protection] [[UNMANAGED_TEMP]]

  // CHECK: apply [[FN]]

  //   Writeback to error temp.
  // CHECK: [[T0:%.*]] = load [trivial] [[UNMANAGED_TEMP]]
  // CHECK: [[T1:%.*]] = unmanaged_to_ref [[T0]]
  // CHECK: [[T1_COPY:%.*]] = copy_value [[T1]]
  // CHECK: [[T1_COPY_DEP:%.*]] = mark_dependence [[T1_COPY]] : $Optional<NSError> on [[ERR_TEMP]]
  // CHECK: assign [[T1_COPY_DEP]] to [[ERR_TEMP]]

  //   Branch on result.
  // CHECK: cond_br {{%.*}}, [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]

  //   Error path: convert NSError → Error and throw.
  // CHECK: [[ERROR_BB]]:
  // CHECK: [[T0:%.*]] = load [take] [[ERR_TEMP]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$s10Foundation22_convertNSErrorToErrorys0E0_pSo0C0CSgF
  // CHECK: [[ERR:%.*]] = apply [[CONVERT]]([[T0]])
  // CHECK: "willThrow"([[ERR]] : $any Error)
  // CHECK: throw [[ERR]] : $any Error
  try c_error_sconce()
}

// --- null_result: nullable pointer return with CFErrorRef ---
// Mirrors NilResult convention (no ObjC equivalent with CFError).

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c19testNullResultCFRefSvyKF : $@convention(thin) () -> (UnsafeMutableRawPointer, @error any Error)
func testNullResultCFRef() throws -> UnsafeMutableRawPointer {
  // CHECK: [[FN:%.*]] = function_ref @$sSo15c_error_cf_nullSvyKFTo : $@convention(c) (Optional<UnsafeMutablePointer<Optional<NSError>>>) -> Optional<UnsafeMutableRawPointer>
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<UnsafeMutablePointer<Optional<NSError>>>) -> Optional<UnsafeMutableRawPointer>

  //   null_result: check result for nil.
  // CHECK: switch_enum [[RESULT]] : $Optional<UnsafeMutableRawPointer>, case #Optional.some!enumelt: [[NORMAL_BB:bb[0-9]+]], case #Optional.none!enumelt: [[ERROR_BB:bb[0-9]+]]
  return try c_error_cf_null()
}

// --- nonnull_error: Void return with CFErrorRef ---
// Verify CFError param is bridged to NSError in the SIL signature.

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c22testNonnullErrorCFRefByyKF : $@convention(thin) () -> @error any Error
func testNonnullErrorCFRefB() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo18c_error_cf_nonnullyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> ()
  // CHECK: apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> ()
  // CHECK: switch_enum {{%.*}} : $Optional<NSError>, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]
  try c_error_cf_nonnull()
}

// --- Trailing block parameter after error ---
// Verify block param is passed correctly when error param is skipped.

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c17testTrailingBlockyyKF : $@convention(thin) () -> @error any Error
func testTrailingBlock() throws {
  //   C function takes (NSError**, block) — verify both params in apply.
  // CHECK: [[FN:%.*]] = function_ref @$sSo22c_error_trailing_blockyyyycSgKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}, {{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>) -> Bool

  //   Bool result with zero_result convention.
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_error_trailing_block {}
}

// --- Block parameter before error (error is last) ---
// Verify block param and int param are passed, error param is at the end.

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c15testBlockBeforeyyKF : $@convention(thin) () -> @error any Error
func testBlockBefore() throws {
  //   C function takes (int, block, NSError**) — verify 3 params in apply.
  // CHECK: [[FN:%.*]] = function_ref @$sSo20c_error_block_beforeyys5Int32V_yycSgtKFTo : $@convention(c) (Int32, Optional<@convention(block) () -> ()>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: apply [[FN]]({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(c) (Int32, Optional<@convention(block) () -> ()>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: cond_br
  try c_error_block_before(1) {}
}

// --- Multiple trailing blocks after error ---
// Verify both block params are passed after the error param.

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c23testMultiTrailingBlocksyyKF : $@convention(thin) () -> @error any Error
func testMultiTrailingBlocks() throws {
  //   C function takes (NSError**, block, block) — verify 3 params.
  // CHECK: [[FN:%.*]] = function_ref @$sSo29c_error_multi_trailing_blocksyyyycSg_ABtKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>, Optional<@convention(block) () -> ()>) -> Bool
  // CHECK: apply [[FN]]({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>, Optional<@convention(block) () -> ()>) -> Bool
  // CHECK: cond_br
  try c_error_multi_trailing_blocks({}, {})
}

// --- Multiple blocks before error (error is last) ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c21testMultiBlocksBeforeyyKF : $@convention(thin) () -> @error any Error
func testMultiBlocksBefore() throws {
  //   C function takes (block, block, NSError**) — verify 3 params.
  // CHECK: function_ref @$sSo27c_error_multi_blocks_beforeyyyycSg_ABtKFTo
  // CHECK: cond_br
  try c_error_multi_blocks_before({}, {})
}

// --- Blocks on both sides of error ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c19testBlocksBothSidesyyKF : $@convention(thin) () -> @error any Error
func testBlocksBothSides() throws {
  //   C function takes (block, NSError**, block) — verify 3 params.
  // CHECK: function_ref @$sSo25c_error_blocks_both_sidesyyyycSg_ABtKFTo
  // CHECK: cond_br
  try c_error_blocks_both_sides({}) {}
}

// --- Multiple parameters before error ---
// Verify non-error params come first, error param in the middle.

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c14testMultiParamyyKF : $@convention(thin) () -> @error any Error
func testMultiParam() throws {
  //   C function takes (int, int, NSError**) — verify 3 params.
  // CHECK: [[FN:%.*]] = function_ref @$sSo19c_error_multi_paramyys5Int32V_ACtKFTo : $@convention(c) (Int32, Int32, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: apply [[FN]]({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(c) (Int32, Int32, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: cond_br
  try c_error_multi_param(1, 2)
}

// --- Boolean type variants: all produce ZeroResult → cond_br on Bool._value ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c18testZeroResultBoolyyKF : $@convention(thin) () -> @error any Error
func testZeroResultBool() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo17c_error_zero_boolyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_error_zero_bool()
}

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c18testZeroResultBOOLyyKF : $@convention(thin) () -> @error any Error
func testZeroResultBOOL() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo17c_error_zero_BOOLyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_error_zero_BOOL()
}

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c21testZeroResultBooleanyyKF : $@convention(thin) () -> @error any Error
func testZeroResultBoolean() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo20c_error_zero_BooleanyyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_error_zero_Boolean()
}

// --- Default heuristic: bool + NSError** → ZeroResult (no swift_error attr) ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c20testDefaultBoolNSErryyKF : $@convention(thin) () -> @error any Error
func testDefaultBoolNSErr() throws {
  // CHECK: [[FN:%.*]] = function_ref @$sSo22c_default_bool_nserroryyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: [[BVAL:%.*]] = struct_extract [[RESULT]] : $Bool, #Bool._value
  // CHECK: cond_br [[BVAL]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_default_bool_nserror()
}

// --- Default heuristic: _Nullable pointer + NSError** → NilResult ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c24testDefaultNullableNSErrSvyKF : $@convention(thin) () -> (UnsafeMutableRawPointer, @error any Error)
func testDefaultNullableNSErr() throws -> UnsafeMutableRawPointer {
  // CHECK: [[FN:%.*]] = function_ref @$sSo26c_default_nullable_nserrorSvyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Optional<UnsafeMutableRawPointer>
  // CHECK: [[RESULT:%.*]] = apply [[FN]]({{%.*}}) : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Optional<UnsafeMutableRawPointer>
  // CHECK: switch_enum [[RESULT]] : $Optional<UnsafeMutableRawPointer>, case #Optional.some!enumelt: [[NORMAL_BB:bb[0-9]+]], case #Optional.none!enumelt: [[ERROR_BB:bb[0-9]+]]
  return try c_default_nullable_nserror()
}

// --- Default heuristic: unannotated pointer + NSError** → NilResult (IUO) ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c27testDefaultUnannotatedNSErrSvyKF : $@convention(thin) () -> (UnsafeMutableRawPointer, @error any Error)
func testDefaultUnannotatedNSErr() throws -> UnsafeMutableRawPointer {
  // CHECK: function_ref @$sSo33c_default_unannotated_ptr_nserrorSvyKFTo : $@convention(c) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Optional<UnsafeMutableRawPointer>
  // CHECK: switch_enum {{%.*}} : $Optional<UnsafeMutableRawPointer>, case #Optional.some!enumelt: [[NORMAL_BB:bb[0-9]+]], case #Optional.none!enumelt: [[ERROR_BB:bb[0-9]+]]
  return try c_default_unannotated_ptr_nserror()
}

// --- Default heuristic: bool + CFErrorRef* with ownership → ZeroResult ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c23testDefaultBoolCFErrOwnyyKF : $@convention(thin) () -> @error any Error
func testDefaultBoolCFErrOwn() throws {
  // CHECK: function_ref @$sSo22c_default_bool_cferroryyKFTo : $@convention(c) (Optional<UnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: cond_br {{%.*}}, [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_default_bool_cferror()
}

// --- Default heuristic: _Bool + int params + NSError** → ZeroResult (c_no_attr) ---

// CHECK-LABEL: sil hidden [ossa] @$s16foreign_errors_c17testNoAttrDefaultyyKF : $@convention(thin) () -> @error any Error
func testNoAttrDefault() throws {
  // CHECK: function_ref @$sSo9c_no_attryys5Int32VKFTo : $@convention(c) (Int32, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>) -> Bool
  // CHECK: cond_br {{%.*}}, [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try c_no_attr(42)
}

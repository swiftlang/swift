// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -swift-version 5 -Xllvm -sil-print-types -emit-sil -primary-file %s -Xllvm -sil-print-after=OSLogOptimization -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
//
// REQUIRES: VENDOR=apple

// REQUIRES: swift_stdlib_no_asserts

// Tests for the OSLogOptimization pass that performs compile-time analysis
// and optimization of the new os log APIs. The tests here check whether specific
// compile-time constants such as the format string, the size of the byte buffer etc. are
// literals after the mandatory pipeline.

import OSLogTestHelper
import Foundation

// CHECK-LABEL: @${{.*}}testSimpleInterpolationyy
func testSimpleInterpolation() {
  _osLogTestHelper("Minimum integer value: \(Int.min)")

  // Check if there is a call to _os_log_impl with a literal format string.
  // CHECK-DAG is used here as it is easier to perform the checks backwards
  // from uses to the definitions.

  // Match the format string first.
  // CHECK: string_literal oslog "Minimum integer value: %ld"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 12
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 8

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 1

  // Check whether argument array is folded. We need not check the contents of
  // the array which is checked by a different test suite.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // We need to wade through some borrows and copy values here.
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 3
}

// CHECK-LABEL: @${{.*}}testInterpolationWithFormatOptionsyy
func testInterpolationWithFormatOptions() {
  _osLogTestHelper("Maximum unsigned integer value: \(UInt.max, format: .hex)")

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "Maximum unsigned integer value: %lx"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 12
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 8

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 1

  // Check whether argument array is folded. We need not check the contents of
  // the array which is checked by a different test suite.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 3
}

// CHECK-LABEL: @${{.*}}testInterpolationWithFormatOptionsAndPrivacyyy
func testInterpolationWithFormatOptionsAndPrivacy() {
  let privateID: UInt = 0x79abcdef
  _osLogTestHelper(
    "Private Identifier: \(privateID, format: .hex, privacy: .private)")

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "Private Identifier: %{private}lx"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 12
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 8

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 1

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 1

  // Check whether argument array is folded. We need not check the contents of
  // the array which is checked by a different test suite.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 3
}

// CHECK-LABEL: @${{.*}}testInterpolationWithMultipleArgumentsyy
func testInterpolationWithMultipleArguments() {
  let privateID = 0x79abcdef
  let filePermissions: UInt = 0o777
  let pid = 122225
  _osLogTestHelper(
    """
    Access prevented: process \(pid, privacy: .public) initiated by \
    user: \(privateID, privacy: .private) attempted resetting \
    permissions to \(filePermissions, format: .octal)
    """)

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "Access prevented: process %{public}ld initiated by user: %{private}ld attempted resetting permissions to %lo"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 32
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 20

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 1

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 3

  // Check whether argument array is folded. We need not check the contents of
  // the array which is checked by a different test suite.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[ARGSARRAYADDR]] = alloc_stack $Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 9
}

// CHECK-LABEL: @${{.*}}testLogMessageWithoutDatayy
func testLogMessageWithoutData() {
  // FIXME: here `ExpressibleByStringLiteral` conformance of OSLogMessage
  // is used. In this case, the constant evaluation begins from the apply of
  // the "string.makeUTF8: initializer. The constant evaluator ends up using
  // the backward mode to identify the string_literal inst passed to the
  // initializer. Eliminate  reliance on this backward mode by starting from
  // the string_literal inst, instead of initialization instruction.
  _osLogTestHelper("A message with no data")

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "A message with no data"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int{{[0-9]+}}, 2

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 0

  // Check whether argument array is folded.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[ARGSARRAY:%[0-9]+]] to {{.*}} 
  // CHECK-DAG: [[ARGSARRAY]] = move_value [var_decl] [[ARGSARRAY:%[0-9]+]]
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 0
}

// CHECK-LABEL: @${{.*}}testEscapingOfPercentsyy
func testEscapingOfPercents() {
  _osLogTestHelper("Process failed after 99% completion")
  // Match the format string first.
  // CHECK: string_literal oslog "Process failed after 99%% completion"
}

// CHECK-LABEL: @${{.*}}testDoublePercentsyy
func testDoublePercents() {
  _osLogTestHelper("Double percents: %%")
  // Match the format string first.
  // CHECK: string_literal oslog "Double percents: %%%%"
}

// CHECK-LABEL: @${{.*}}testSmallFormatStringsyy
func testSmallFormatStrings() {
  _osLogTestHelper("a")
  // Match the format string first.
  // CHECK: string_literal oslog "a"
}

/// A stress test that checks whether the optimizer handle messages with more
/// than 48 interpolated expressions. Interpolated expressions beyond this
/// limit must be ignored.
// CHECK-LABEL: @${{.*}}testMessageWithTooManyArgumentsyy
func testMessageWithTooManyArguments() {
  _osLogTestHelper(
    """
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(48) \(49)
    """)

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld "

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 482
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 290

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 48

  // Check whether argument array is folded.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 144
}

// CHECK-LABEL: @${{.*}}testInt32Interpolationyy
func testInt32Interpolation() {
  _osLogTestHelper("32-bit integer value: \(Int32.min)")

  // Check if there is a call to _os_log_impl with a literal format string.

  // Match the format string first.
  // CHECK: string_literal oslog "32-bit integer value: %d"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 8
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 8

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 1
}

// CHECK-LABEL: @${{.*}}testDynamicStringArgumentsyy
func testDynamicStringArguments() {
  let concatString = "hello" + " - " + "world"
  let interpolatedString = "\(31) trillion digits of pi are known so far"

  _osLogTestHelper(
    """
    concat: \(concatString, privacy: .public) \
    interpolated: \(interpolatedString, privacy: .private)
    """)

  // Check if there is a call to _os_log_impl with a literal format string.
  // CHECK-DAG is used here as it is easier to perform the checks backwards
  // from uses to the definitions.

  // Match the format string first.
  // CHECK: string_literal oslog "concat: %{public}s interpolated: %{private}s"

  // Check if the size of the argument buffer is a constant.

  // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
  // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
  // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 22
  // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 14

  // Check whether the header bytes: preamble and argument count are constants.

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 3

  // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
  // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
  // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
  // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 2

  // Check whether argument array is folded. We need not check the contents of
  // the array which is checked by a different test suite.

  // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
  // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
  // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
  // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
  // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
  // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
  // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
  // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
  // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 6
}

// CHECK-LABEL: @${{.*}}testNSObjectInterpolationyy
func testNSObjectInterpolation() {
  let nsArray: NSArray = [0, 1, 2]
  let nsDictionary: NSDictionary = [1 : ""]
  _osLogTestHelper(
    """
    NSArray: \(nsArray, privacy: .public) \
    NSDictionary: \(nsDictionary, privacy: .private)
    """)
    // Check if there is a call to _os_log_impl with a literal format string.
    // CHECK-DAG is used here as it is easier to perform the checks backwards
    // from uses to the definitions.

    // Match the format string first.
    // CHECK: string_literal oslog "NSArray: %{public}@ NSDictionary: %{private}@"

    // Check if the size of the argument buffer is a constant.

    // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
    // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
    // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 22
    // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 14

    // Check whether the header bytes: preamble and argument count are constants.

    // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
    // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
    // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 3

    // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
    // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
    // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 2

    // Check whether argument array is folded. We need not check the contents of
    // the array which is checked by a different test suite.

    // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
    // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
    // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
    // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
    // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
    // CHECK-DAG: [[FINARR]] = apply {{.*}}<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
    // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
    // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
    // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
    // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 6
}

// CHECK-LABEL: @${{.*}}testDoubleInterpolationyy
func testDoubleInterpolation() {
  let twoPi = 2 * 3.14
  _osLogTestHelper("Tau = \(twoPi)")
    // Check if there is a call to _os_log_impl with a literal format string.
    // CHECK-DAG is used here as it is easier to perform the checks backwards
    // from uses to the definitions.

    // Match the format string first.
    // CHECK: string_literal oslog "Tau = %f"

    // Check if the size of the argument buffer is a constant.

    // CHECK-DAG: [[ALLOCATE:%[0-9]+]] = function_ref @$sSp8allocate8capacitySpyxGSi_tFZ
    // CHECK-DAG: apply [[ALLOCATE]]<UInt8>([[BUFFERSIZE:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[BUFFERSIZE]] = struct $Int ([[BUFFERSIZELIT:%[0-9]+]]
    // CHECK-64-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int64, 12
    // CHECK-32-DAG: [[BUFFERSIZELIT]] = integer_literal $Builtin.Int32, 12

    // Check whether the header bytes: preamble and argument count are constants.

    // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
    // CHECK-DAG: apply [[SERIALIZE]]([[PREAMBLE:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[PREAMBLE]] =  struct $UInt8 ([[PREAMBLELIT:%[0-9]+]] : $Builtin.Int8)
    // CHECK-DAG: [[PREAMBLELIT]] = integer_literal $Builtin.Int8, 0

    // CHECK-DAG: [[SERIALIZE:%[0-9]+]] = function_ref @$s15OSLogTestHelper9serialize_2atys5UInt8V_SpyAEGztF
    // CHECK-DAG: apply [[SERIALIZE]]([[ARGCOUNT:%[0-9]+]], {{%.*}})
    // CHECK-DAG: [[ARGCOUNT]] =  struct $UInt8 ([[ARGCOUNTLIT:%[0-9]+]] : $Builtin.Int8)
    // CHECK-DAG: [[ARGCOUNTLIT]] = integer_literal $Builtin.Int8, 1

    // Check whether argument array is folded. The contents of the array is
    // not checked here, but is checked by a different test suite.

    // CHECK-DAG: [[FOREACH:%[0-9]+]] = function_ref @$sSTsE7forEachyyy7ElementQzKXEKF
    // CHECK-DAG: try_apply [[FOREACH]]<Array<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>>({{%.*}}, [[SB:%[0-9]+]])
    // CHECK-DAG: [[SB]] = store_borrow [[FINARR:%[0-9]+]] to [[ARGSARRAYADDR:%[0-9]+]]
    // CHECK-DAG: [[FINARR]] = move_value [var_decl] [[FINARR:%[0-9]+]]
    // CHECK-DAG: [[FINARRFUNC:%[0-9]+]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
    // CHECK-DAG: [[FINARR]] = apply [[FINARRFUNC]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARGSARRAY:%[0-9]+]])
    // CHECK-DAG: ([[ARGSARRAY]], {{%.*}}) = destructure_tuple [[ARRAYINITRES:%[0-9]+]]
    // CHECK-DAG: [[ARRAYINITRES]] = apply [[ARRAYINIT:%[0-9]+]]<(inout UnsafeMutablePointer<UInt8>, inout Optional<UnsafeMutablePointer<NSObject>>, inout Optional<UnsafeMutablePointer<Any>>) -> ()>([[ARRAYSIZE:%[0-9]+]])
    // CHECK-DAG: [[ARRAYINIT]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
    // CHECK-DAG: [[ARRAYSIZE]] = integer_literal $Builtin.Word, 3
}

// CHECK-LABEL: @${{.*}}testDeadCodeElimination6number8num32bit6stringySi_s5Int32VSStF
func testDeadCodeElimination(
  number: Int,
  num32bit: Int32,
  string: String
) {
  _osLogTestHelper("A message with no data")
  _osLogTestHelper("smallstring")
  _osLogTestHelper(
    """
    A message with many interpolations \(number), \(num32bit), \(string), \
    and a suffix
    """)
  _osLogTestHelper(
    """
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(48) \(49)
    """)
  let concatString = string + ":" + String(number)
  _osLogTestHelper("\(concatString)")
    // CHECK-NOT: OSLogMessage
    // CHECK-NOT: OSLogInterpolation
    // CHECK-LABEL: end sil function '${{.*}}testDeadCodeElimination6number8num32bit6stringySi_s5Int32VSStF
}

protocol Proto {
  var property: String { get set }
}

// Test capturing of address-only types in autoclosures. The following tests check that
// there are no crashes in these cases.

// CHECK-LABEL: @${{.*}}testInterpolationOfExistentials1pyAA5Proto_p_tF
func testInterpolationOfExistentials(p: Proto) {
  _osLogTestHelper("A protocol's property \(p.property)")
}

// CHECK-LABEL: @${{.*}}testInterpolationOfGenerics1pyx_tAA5ProtoRzlF
func testInterpolationOfGenerics<T : Proto>(p: T) {
  _osLogTestHelper("A generic argument's property \(p.property)")
}

class TestClassSelfTypeCapture {
  // CHECK-LABEL: @${{.*}}TestClassSelfTypeCaptureC04testdeF0yyF
  func testSelfTypeCapture() {
    _osLogTestHelper("Self type of a class \(String(describing: Self.self))")
  }
}

struct TestStructSelfTypeCapture {
  // CHECK-LABEL: @${{.*}}TestStructSelfTypeCaptureV04testdeF0yyF
  func testSelfTypeCapture() {
    _osLogTestHelper("Self type of a struct \(String(describing: Self.self))")
  }
}

protocol TestProtocolSelfTypeCapture {
  func testSelfTypeCapture()
}

extension TestProtocolSelfTypeCapture {
  // CHECK-LABEL: @${{.*}}TestProtocolSelfTypeCapturePAAE04testdeF0yyF
  func testSelfTypeCapture() {
    _osLogTestHelper("Self type of a protocol \(String(describing: Self.self))")
  }
}

// Test that SwiftUI's preview transformations work with the logging APIs.

// A function similar to the one used by SwiftUI preview to wrap string
// literals.
@_semantics("constant_evaluable")
@_transparent
public func __designTimeStringStub(
  _ key: String,
  fallback: OSLogMessage
) -> OSLogMessage {
  fallback
}

// CHECK-LABEL: @${{.*}}testSwiftUIPreviewWrappingyy
func testSwiftUIPreviewWrapping() {
  _osLogTestHelper(__designTimeStringStub("key", fallback: "percent: %"))
    // CHECK: string_literal oslog "percent: %%"
    // CHECK-NOT: OSLogMessage
    // CHECK-NOT: OSLogInterpolation
    // CHECK-LABEL: end sil function '${{.*}}testSwiftUIPreviewWrappingyy
}


func functionTakingClosure(_ x: () -> Void) { }

func testWrappingWithinClosures(x: Int) {
  functionTakingClosure {
    _osLogTestHelper(
      __designTimeStringStub(
        "key",
        fallback: "escaping of percent: %"))
      // CHECK-LABEL: @${{.*}}testWrappingWithinClosures1xySi_tFyyXEfU_
      // CHECK: string_literal oslog "escaping of percent: %%"
      // CHECK-NOT: OSLogMessage
      // CHECK-NOT: OSLogInterpolation
      // CHECK-LABEL: end sil function '${{.*}}testWrappingWithinClosures1xySi_tFyyXEfU_
  }
}

func testAnimationSignpost(cond: Bool, x: Int, y: Float) {
  _osSignpostAnimationBeginTestHelper("animation begins here %d", Int.min)
  _osSignpostAnimationBeginTestHelper("a message without arguments")
  _osSignpostAnimationBeginTestHelper("animation begins here %ld", x)
  _osSignpostAnimationBeginTestHelper("animation begins here %ld", cond ? x : y)
  _osSignpostAnimationBeginTestHelper("animation begins here %ld %f", x, y)
  // CHECK-LABEL: @${{.*}}testAnimationSignpost4cond1x1yySb_SiSftF
  // CHECK: string_literal oslog "animation begins here %d isAnimation=YES"
  // CHECK: string_literal oslog "a message without arguments isAnimation=YES"
  // CHECK: string_literal oslog "animation begins here %ld isAnimation=YES"
  // CHECK: string_literal oslog "animation begins here %ld isAnimation=YES"
  // CHECK: string_literal oslog "animation begins here %ld %f isAnimation=YES"
}

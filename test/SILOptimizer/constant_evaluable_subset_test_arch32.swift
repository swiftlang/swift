// REQUIRES: PTRSIZE=32
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -primary-file %s -o %t/constant_evaluable_subset_test_arch32_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and test the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: not %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 3000 -test-constant-evaluable-subset %t/constant_evaluable_subset_test_arch32_silgen.sil > /dev/null 2> %t/error-output
//
// RUN: %FileCheck %s < %t/error-output

// Test Swift code snippets that are expected to be constant evaluable and those
// that are not. If any of the test here fails, it indicates a change in the
// output of SILGen or the mandatory passes that affects the constant
// evaluability of the corresponding Swift code. The tests here are specific
// to 32bit architecture.

// Unfortunately, on 32bit architectures the following operations are not
// constant evaluable, as it requires supporting co-routines in the evaluator.
// They are however constant evaluable on 64bit architectures. This difference
// arises because in 32bit architecture Int64 occupies multiple words and
// the following operations involves iterating through the words in Int64. This
// causes the interpreted code to change between 64bit and 32bit architectures.

// CHECK-LABEL: @testIntExtensions
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testIntExtensions(a: Int8, b: Int16, c: Int32) -> Int64 {
  return Int64(a) + Int64(b) + Int64(c)
}

@_semantics("test_driver")
internal func interpretIntExtensions() -> Int64 {
  return testIntExtensions(a: 100, b: -130, c: -50000)
}

// CHECK-LABEL: @testUIntExtensions
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testUIntExtensions(a: UInt8, b: UInt16, c: UInt32) -> UInt64 {
  return UInt64(a) + UInt64(b) + UInt64(c)
}

@_semantics("test_driver")
internal func interpretUIntExtensions() -> UInt64 {
  return testUIntExtensions(a: 100, b: 130, c: 0xffffffff)
}

// CHECK-LABEL: @testIntTruncations
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testIntTruncations(a: Int64) -> Int16 {
  return Int16(a)
}

@_semantics("test_driver")
internal func interpretIntTruncations() -> Int16 {
  return testIntTruncations(a: 100)
}

// CHECK-LABEL: @testInvalidIntTruncations
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testInvalidIntTruncations(a: Int64) -> Int8 {
  return Int8(a)
}

@_semantics("test_driver")
internal func interpretInvalidIntTruncations() -> Int8 {
  return testInvalidIntTruncations(a: 130)
}

// CHECK-LABEL: @testUIntTruncations
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testUIntTruncations(a: UInt64) -> UInt16 {
  return UInt16(a)
}

@_semantics("test_driver")
internal func interpretUIntTruncations() -> UInt16 {
  return testUIntTruncations(a: 100)
}

// CHECK-LABEL: @testSingedUnsignedConversions
// CHECK: error: not constant evaluable
// CHECK: note: encountered operation not supported by the evaluator: begin_apply
@_semantics("constant_evaluable")
internal func testSingedUnsignedConversions(a: Int64, b: UInt8) -> UInt64 {
  return UInt64(a) + UInt64(Int8(b))
}

@_semantics("test_driver")
internal func interpretSingedUnsignedConversions() -> UInt64 {
  return testSingedUnsignedConversions(a: 100, b: 120)
}

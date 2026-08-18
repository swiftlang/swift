// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -parse-as-library -verify -verify-ignore-unrelated %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -O -parse-as-library -DEMIT_SIL %s

// REQUIRES: objc_interop

import Foundation
import swift_error_c_functions

// Test swift_error conventions on C functions.
// Each case mirrors the ObjC method tests in foreign_errors.swift / errors.h.

func testSwiftError() throws {
  let _: Bool = try c_error_bound()
  let _: Float = try c_error_bounce()
  let _: () = try c_error_flounce()
  let _: CInt = try c_error_ounce()
  let _: () = try c_error_once()
  let _: () = try c_error_sconce()
  let _: () = try c_error_scotch()

  var err: NSError?
  let _: Bool = c_error_scout(&err)
}

// All boolean-like C types should produce ZeroResult (throwing returns Void).
func testBooleanTypes() throws {
  let _: () = try c_error_zero_bool()
  let _: () = try c_error_zero_BOOL()
  let _: () = try c_error_zero_Boolean()

  let _: () = try c_error_nonzero_bool()
  let _: () = try c_error_nonzero_BOOL()
  let _: () = try c_error_nonzero_Boolean()
}

func testCFError() throws {
  let _: UnsafeMutableRawPointer = try c_error_cf_null()
  let _: () = try c_error_cf_nonnull()
}

func testNullResultNSError() throws {
  let _: UnsafeMutableRawPointer = try c_error_ns_null()
}

func testTrailingBlock() throws {
  try c_error_trailing_block {}
}

func testBlockBefore() throws {
  try c_error_block_before(1) {}
}

func testMultiTrailingBlocks() throws {
  try c_error_multi_trailing_blocks({}, {})
}

func testMultiBlocksBefore() throws {
  try c_error_multi_blocks_before({}, {})
}

func testBlocksBothSides() throws {
  try c_error_blocks_both_sides({}) {}
}

func testMultiParam() throws {
  try c_error_multi_param(1, 2)
}

func testNonThrowingVariants() {
  var nserr: NSError?
  var cferr: CFError?

  let _: Bool = c_error_bound(&nserr)
  let _: Float = c_error_bounce(&nserr)
  c_error_flounce(&nserr)
  let _: CInt = c_error_ounce(&nserr)
  let _: CInt = c_error_once(&nserr)
  let _: Bool = c_error_sconce(&nserr)
  let _: Bool = c_error_scotch(&nserr)

  let _: UnsafeMutableRawPointer? = c_error_cf_null(&cferr)
  c_error_cf_nonnull(&cferr)

  let _: UnsafeMutableRawPointer? = c_error_ns_null(&nserr)

  let _: Bool = c_error_trailing_block(&nserr) {}
  let _: Bool = c_error_block_before(1, {}, &nserr)
  let _: Bool = c_error_multi_trailing_blocks(&nserr, {}, {})
  let _: Bool = c_error_multi_blocks_before({}, {}, &nserr)
  let _: Bool = c_error_blocks_both_sides({}, &nserr) {}
  let _: Bool = c_error_multi_param(1, 2, &nserr)
}

// --- Default heuristics (no swift_error attribute) ---

func testDefaultBoolNSError() throws {
  let _: () = try c_default_bool_nserror()
}

func testDefaultNullableNSError() throws {
  let _: UnsafeMutableRawPointer = try c_default_nullable_nserror()
}

func testDefaultUnannotatedPtrNSError() throws {
  let _: UnsafeMutableRawPointer = try c_default_unannotated_ptr_nserror()
}

func testDefaultBoolCFError() throws {
  let _: () = try c_default_bool_cferror()
}

// c_no_attr now gets a throwing variant via default heuristics.
func testNoAttrNowThrows() throws {
  try c_no_attr(42)
}

func testDefaultNonThrowingVariants() {
  var nserr: NSError?
  var cferr: CFError?

  let _: Bool = c_default_bool_nserror(&nserr)
  let _: UnsafeMutableRawPointer? = c_default_nullable_nserror(&nserr)
  let _: UnsafeMutableRawPointer! = c_default_unannotated_ptr_nserror(&nserr)
  let _: Bool = c_default_bool_cferror(&cferr)
  let _: Bool = c_no_attr(42, &nserr)

  // swift_error(null_result) on _Nonnull falls back to non-throwing.
  let _: UnsafeMutableRawPointer = c_error_null_nonnull(&nserr)
}

// A C function mapped onto a type by swift_name keeps its error parameter.
func testMemberKeepsErrorParameter() {
  let widget = CErrorWidget()
  var nserr: NSError?
  let _: Bool = widget.doIt(error: &nserr)

  var point = CErrorPoint(x: 0, y: 0)
  let _: Bool = point.doIt(error: &nserr)
}

// The ObjC error convention covers NSError** only, so a CFErrorRef*
// out-parameter on a method stays an ordinary unmanaged parameter.
func testObjCMethodCFErrorKeepsParameter() {
  let widget = CErrorObjCWidget()
  var cferr: Unmanaged<CFError>?
  let _: Bool = widget.doIt(&cferr)
}

// Dropping the error parameter keeps the remaining argument labels aligned.
func testLabelsAfterErrorParameter() throws {
  try labeledTrailingBlock(callback: {})
}

// A lifetime annotation on the dropped error parameter is not carried over.
func testLifetimeboundErrorParameter() throws {
  let _: UnsafeMutableRawPointer = try c_error_lifetimebound()
}

// Any CF type bridged to NSError carries the error convention, not just CFError.
func testCustomCFErrorType() throws {
  let _: () = try c_default_bool_custom_cferror()
}

#if !EMIT_SIL
func testNoneNoThrow() throws {
  try c_error_scout() // expected-error {{missing argument for parameter #1 in call}}
}

func testCFBareNoThrow() throws {
  try c_error_cf_bare() // expected-error {{missing argument for parameter #1 in call}}
}

func testNoAttrCFNoThrow() throws {
  try c_no_attr_cf(42) // expected-error {{missing argument for parameter #2 in call}}
}

func testDefaultNegativeInt() throws {
  try c_default_int_nserror(42) // expected-error {{cannot convert value of type 'Int' to expected argument type 'AutoreleasingUnsafeMutablePointer<NSError?>'}}
}

func testDefaultNegativeCFBare() throws {
  try c_default_bool_cferror_bare() // expected-error {{missing argument for parameter #1 in call}}
}

func testDefaultNegativeErrorUnion() throws {
  try c_default_bool_error_union() // expected-error {{missing argument for parameter #1 in call}}
}

// A throwing variant that takes the name of another declaration leaves both
// visible, so the name is ambiguous rather than silently resolved.
func testCollidingName() throws {
  try collide(x: 1) // expected-error {{ambiguous use of 'collide(x:)'}}
}

func testDefaultNegativeOverrideNone() throws {
  try c_default_override_none() // expected-error {{missing argument for parameter #1 in call}}
}

func testDefaultNegativeNonnull() throws {
  try c_default_nonnull_nserror() // expected-error {{missing argument for parameter #1 in call}}
}

func testDefaultNegativeVoid() {
  c_default_void_nserror() // expected-error {{missing argument for parameter #1 in call}}
}

// --- swift_error(null_result) on a non-optional pointer ---
// Clang accepts the attribute (the return is still a pointer), but the Swift
// importer rejects it because the result is non-optional. The function should
// still be callable as the non-throwing import.

func testNullResultNonnullFallback() throws {
  try c_error_null_nonnull() // expected-error {{missing argument for parameter #1 in call}}
}

func testUnhandledError() {
  try c_error_sconce() // expected-error {{errors thrown from here are not handled}}
}

func testDefaultUnhandledError() {
  try c_default_bool_nserror() // expected-error {{errors thrown from here are not handled}}
}

func testZeroResultIsVoid() throws {
  let _: Bool = try c_error_sconce() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_zero_bool() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_zero_BOOL() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_zero_Boolean() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_nonzero_bool() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_nonzero_BOOL() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_error_nonzero_Boolean() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
  let _: Bool = try c_default_bool_nserror() // expected-error {{cannot convert value of type '()' to specified type 'Bool'}}
}
#endif

// RUN: %target-swift-frontend -typecheck -I %S/Inputs %s -enable-experimental-cxx-interop -verify

// REQUIRES: objc_interop

// expected-no-diagnostics

import ConstexprMacros

// Complex integer expressions
func testComplexIntegerExpressions() {
  let _: CInt = COMPLEX_BITWISE   // (1 << 4 | 1 << 5 | 1 << 6) = 112
  let _: CInt = CHAINED_ADD       // 10 + 20 + 30 = 60
  let _: CInt = MODULO_EXPR       // 100 % 7 = 2
  let _: CInt = TERNARY_EXPR      // 10 > 5 ? 10 : 5 = 10
  let _: CChar = CHAR_LITERAL     // 'A' = 65
  let _: CInt = NESTED_PARENS     // ((((42)))) = 42
  let _: CInt = MULTI_OP          // 3 * 4 + 5 = 17
  let _: CInt = SHIFT_AND_OR      // (1<<0)|(1<<1)|(1<<2) = 7
  let _: CInt = COMPLEX_ARITH     // (100-50)*2 + 10/5 = 102
  let _: CInt = CHAINED_SHIFT     // 1 << 2 << 1 = 8
  let _: CInt = NEGATION_EXPR     // ~0 & 0xFF = 255
}

// Macro chains
func testMacroChains() {
  let _: CInt = BASE_VAL           // 100
  let _: CInt = DERIVED_VAL        // 150
  let _: CInt = CHAINED_MACRO_EXPR // 100*2 + 150 = 350
}

// Boolean / comparison results
func testBooleanResults() {
  let _: Bool = CMP_EQUAL          // 42 == 42
  let _: Bool = CMP_NOT_EQUAL      // 1 != 2
  let _: Bool = CMP_LESS           // 1 < 2
  let _: Bool = CMP_GREATER        // 10 > 5
  let _: Bool = LOGICAL_AND        // 1 && 1
  let _: Bool = LOGICAL_OR         // 0 || 1
  let _: Bool = LOGICAL_COMPLEX    // (1 < 2) && (3 > 0)
  let _: Bool = LOGICAL_NOT        // !0
  let _: Bool = LOGICAL_NOT_EXPR   // !(1 > 2)

  // Verify these are actually Bool, not Int32.
  if CMP_EQUAL && LOGICAL_AND {}
}

// Unsigned type preservation
func testUnsignedTypes() {
  let _: CUnsignedInt = UNSIGNED_VAL       // 42U
  let _: CUnsignedLong = UNSIGNED_LONG_VAL // 100UL
  let _: CUnsignedInt = UNSIGNED_EXPR      // 10U + 20U = 30
}

// Float expressions
func testFloatExpressions() {
  let _: CDouble = FLOAT_LITERAL   // 3.14
  let _: CDouble = FLOAT_EXPR      // 1.5 + 2.5 = 4.0
  let _: CDouble = FLOAT_NEG       // -1.5
  let _: CDouble = FLOAT_COMPLEX   // 3.0*2.0 + 1.0 = 7.0
}

// Type casts
func testTypeCasts() {
  let _: CUnsignedInt = CAST_UNSIGNED    // (unsigned)42
  let _: CLongLong = CAST_LONG_LONG      // (long long)1
}

// Function-like macros in replacement text
func testFuncLikeInBody() {
  let _: CInt = USES_FUNC_LIKE           // FUNC_LIKE() = 42
  let _: CInt = USES_FUNC_LIKE_ARG       // FUNC_LIKE_ARG(21) = 42
  let _: CInt = EXPR_WITH_FUNC_LIKE      // 1 + FUNC_LIKE() = 43
}

// Fancy errors
func testFancyErrors() {
  let _: CInt = SYS_FANCY        // ERR_SYS(0x38)
  let _: CInt = SUB_FANCY_COMMON // ERR_SUB(0)
  let _: CInt = FANCY_BAD_ARG    // COMMON_ERR(0x2c2)
}

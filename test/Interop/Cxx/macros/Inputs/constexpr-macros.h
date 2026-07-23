#ifndef CONSTEXPR_MACROS_H
#define CONSTEXPR_MACROS_H

// Complex integer expressions
#define COMPLEX_BITWISE (1 << 4 | 1 << 5 | 1 << 6)
#define CHAINED_ADD (10 + 20 + 30)
#define MODULO_EXPR (100 % 7)
#define TERNARY_EXPR (10 > 5 ? 10 : 5)
#define CHAR_LITERAL ('A')
#define NESTED_PARENS ((((42))))
#define MULTI_OP (3 * 4 + 5)
#define SHIFT_AND_OR ((1 << 0) | (1 << 1) | (1 << 2))
#define COMPLEX_ARITH ((100 - 50) * 2 + 10 / 5)
#define CHAINED_SHIFT (1 << 2 << 1)
#define NEGATION_EXPR (~0 & 0xFF)

// Macro chains
#define BASE_VAL 100
#define DERIVED_VAL (BASE_VAL + 50)
#define CHAINED_MACRO_EXPR (BASE_VAL * 2 + DERIVED_VAL)

// Boolean / comparison results
#define CMP_EQUAL (42 == 42)
#define CMP_NOT_EQUAL (1 != 2)
#define CMP_LESS (1 < 2)
#define CMP_GREATER (10 > 5)
#define LOGICAL_AND (1 && 1)
#define LOGICAL_OR (0 || 1)
#define LOGICAL_COMPLEX ((1 < 2) && (3 > 0))
#define LOGICAL_NOT (!0)
#define LOGICAL_NOT_EXPR (!(1 > 2))

// Unsigned type preservation
#define UNSIGNED_VAL 42U
#define UNSIGNED_LONG_VAL 100UL
#define UNSIGNED_EXPR (10U + 20U)

// Float expressions
#define FLOAT_LITERAL 3.14
#define FLOAT_EXPR (1.5 + 2.5)
#define FLOAT_NEG (-1.5)
#define FLOAT_COMPLEX (3.0 * 2.0 + 1.0)

// Type casts
#define CAST_UNSIGNED ((unsigned)42)
#define CAST_LONG_LONG ((long long)1)

// Function-like macros in replacement text
#define FUNC_LIKE() 42
#define FUNC_LIKE_ARG(x) ((x) * 2)
#define USES_FUNC_LIKE FUNC_LIKE()
#define USES_FUNC_LIKE_ARG FUNC_LIKE_ARG(21)
#define EXPR_WITH_FUNC_LIKE (1 + FUNC_LIKE())

// Fancy errors
#define ERR_SYS(x) ((signed)((((unsigned)(x)) & 0x3f) << 26))
#define ERR_SUB(x) (((x) & 0xfff) << 14)
#define SYS_FANCY ERR_SYS(0x38)
#define SUB_FANCY_COMMON ERR_SUB(0)
#define COMMON_ERR(return) (SYS_FANCY | SUB_FANCY_COMMON | return)
#define FANCY_BAD_ARG COMMON_ERR(0x2c2)

// Negative cases: should NOT be importable
#define EMPTY_MACRO
#define FUNC_LIKE_MACRO_ONLY(x) ((x) + 1)

// Negative case: LValue to constexpr var
constexpr int kConstexprVar = 99;
#define MACRO_REF_CONSTEXPR kConstexprVar

// Negative case: Enum constant
enum Color { Red = 0, Green = 1, Blue = 2 };
#define MACRO_REF_ENUM Green

#endif

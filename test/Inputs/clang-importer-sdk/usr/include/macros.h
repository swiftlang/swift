#include <macros_impl.h>
#include <macros_private_impl.h>

// Get Clang's NULL.
#include <stddef.h>

#define M_PI 3.14159265358979
#define M_PIf 3.14159265358979F
#define GL_FALSE 0
#define GL_RGB 0x1907
#define GL_RGBA 0x1908
#define EOF (-1)
#define UINT32_MAX 0xFFFFFFFFU
#define INT64_MAX 0x7FFFFFFFFFFFFFFFLL
#define UINT64_MAX 0xFFFFFFFFFFFFFFFFLL
#define MINUS_THREE -3
#define true 1
#define false 0
#define TRUE 1
#define FALSE 0

#define A_PI M_PI

#define VERSION_STRING "Swift 1.0"
#define UTF8_STRING u8"Swift 🏃"
#define OBJC_STRING @"Unicode! ✨"
#define CF_STRING CFSTR("Swift")

#define INVALID_INTEGER_LITERAL_1 10_9
#define INVALID_INTEGER_LITERAL_2 10abc
#define VALID_INTEGER_LITERAL_FUNC() 1
#define VALID_NSSTRING_LITERAL_FUNC() @"function-like macro"
#define INTEGER_LITERAL_FUNC_FUNC() VALID_INTEGER_LITERAL_FUNC()
#define NSSTRING_LITERAL_FUNC_FUNC() VALID_NSSTRING_LITERAL_FUNC()

// Check that macros that expand to macros from another module are imported
// correctly.
#define USES_MACRO_FROM_OTHER_MODULE_1 MACRO_FROM_IMPL
#define USES_MACRO_FROM_OTHER_MODULE_2 MACRO_FROM_PRIVATE_IMPL
#define USES_MACRO_FROM_OTHER_MODULE_1_FUNC() MACRO_FROM_IMPL
#define USES_MACRO_FROM_OTHER_MODULE_2_FUNC() MACRO_FROM_PRIVATE_IMPL

// Should be suppressed during import.
#define NS_BLOCKS_AVAILABLE 1
#define CF_USE_OSBYTEORDER_H 1

#define NULL_VIA_NAME (NULL)
#define NULL_VIA_VALUE ((void *) 0)

#ifndef nil
# define nil ((id)0)
#endif
#define NULL_AS_NIL nil

#ifndef Nil
# define Nil ((Class)0)
#endif
#define NULL_AS_CLASS_NIL Nil

#define RECURSION RECURSION
#define REF_TO_RECURSION RECURSION

#define DISPATCH_TIME_NOW (0ull)
#define DISPATCH_TIME_FOREVER (~0ull)

// Bitwise Operations.

#define BIT_SHIFT_1 (1 << 0)
#define BIT_SHIFT_2 (1 << 2)
#define BIT_SHIFT_3 (3LL << 3)
#define BIT_SHIFT_4 (1U << 1)

#define STARTPOS_ATTRS 8
#define ATTR_BOLD      (1 << STARTPOS_ATTRS)
#define ATTR_ITALIC    (2 << STARTPOS_ATTRS)
#define ATTR_UNDERLINE (4 << STARTPOS_ATTRS)
#define ATTR_INVALID   (4 << MINUS_THREE) // Should skip. Negative shift.

#define RSHIFT_ONE     (UINT32_MAX >> 31)
#define RSHIFT_NEG     (MINUS_THREE >> 1)
#define RSHIFT_INVALID (0xFFFF >> MINUS_THREE) // Should skip. Negative shift.

#define XOR_HIGH (UINT64_MAX ^ UINT32_MAX)

// Integer Arithmetic.

#define ADD_ZERO        (0 + 0)
#define ADD_ONE         (ADD_ZERO + 1)
#define ADD_TWO         (ADD_ONE + ADD_ONE)
#define ADD_MINUS_TWO   (1 + MINUS_THREE)
#define ADD_MIXED_WIDTH (3 + 166LL)        // Result = 169LL.
#define ADD_MIXED_SIGN  (42U + 100LL)      // Result = 142LL.
#define ADD_UNDERFLOW   (0U + MINUS_THREE) // Result = (UINT32_MAX - 2)U.
#define ADD_OVERFLOW    (UINT32_MAX + 3)   // Result = 2U.

#define SUB_ONE         (ADD_TWO - 1)
#define SUB_ZERO        (SUB_ONE - SUB_ONE)
#define SUB_MINUS_ONE   (SUB_ZERO - SUB_ONE)
#define SUB_MIXED_WIDTH (64 - 22LL)                  // Result = 42LL.
#define SUB_MIXED_SIGN  (100U - 49)                  // Result = 51U.
#define SUB_UNDERFLOW   (0U - 1)                     // Result = (UINT32_MAX)U.
#define SUB_OVERFLOW    (UINT32_MAX - ADD_MINUS_TWO) // Result = 1U.

#define MULT_POS      (12 * 3)
#define MULT_NEG      (4 * MINUS_THREE)
#define MULT_MIXED_TYPES (2LL * UINT32_MAX) // Result = (2^33)LL

#define DIVIDE_INTEGRAL    (1024 / 8)
#define DIVIDE_NONINTEGRAL (3 / 2)
#define DIVIDE_MIXED_TYPES  (INT64_MAX / UINT32_MAX) // Result = (2^31)LL
#define DIVIDE_INVALID     (69 / 0) // Should skip. Divide by zero.

// Integer Comparisons.

#define EQUAL_FALSE            (99 == 66)
#define EQUAL_TRUE             (SUB_ONE == 1)
#define EQUAL_TRUE_MIXED_TYPES (UINT32_MAX == 4294967295LL)

#define GT_FALSE  (SUB_ONE > 50)
#define GT_TRUE   (INT64_MAX > UINT32_MAX)
#define MINUS_FIFTY (0 - 50)
#define GTE_FALSE (MINUS_FIFTY >= 50)
#define GTE_TRUE  (INT64_MAX >= UINT32_MAX)

#define LT_FALSE  (SUB_UNDERFLOW < UINT32_MAX)
#define MINUS_NINETY_NINE (-99)
#define MINUS_FORTY_TWO   (-42)
#define LT_TRUE   (MINUS_NINETY_NINE < MINUS_FORTY_TWO)
#define LTE_FALSE (ADD_ONE <= 0)
#define LTE_TRUE  (SUB_UNDERFLOW <= UINT32_MAX)

// Logical Comparisons

#define L_AND_TRUE    (1 && 1)
#define L_AND_FALSE   (0 && 1)
#define L_AND_TRUE_B  (EQUAL_TRUE && EQUAL_TRUE_MIXED_TYPES)
#define L_AND_FALSE_B (EQUAL_FALSE && EQUAL_TRUE)

#define L_OR_TRUE     (0 || 1)
#define L_OR_FALSE    (0 || 0)
#define L_OR_TRUE_B   (EQUAL_FALSE || EQUAL_TRUE)
#define L_OR_FALSE_B  (EQUAL_FALSE || L_OR_FALSE)

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

// Check that macros that expand to macros from another module are imported
// correctly.
#define USES_MACRO_FROM_OTHER_MODULE_1 MACRO_FROM_IMPL
#define USES_MACRO_FROM_OTHER_MODULE_2 MACRO_FROM_PRIVATE_IMPL

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


#define DISPATCH_TIME_NOW (0ull)
#define DISPATCH_TIME_FOREVER (~0ull)

#define BIT_SHIFT_1 (1 << 0)
#define BIT_SHIFT_2 (1 << 2)
#define BIT_SHIFT_3 (3LL << 3)
#define BIT_SHIFT_4 (1U << 1)

#define RECURSION RECURSION
#define REF_TO_RECURSION RECURSION

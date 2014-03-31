#include <macros_impl.h>
#include <macros_private_impl.h>

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

#define A_PI M_PI

#define VERSION_STRING "Swift 1.0"
#define UTF8_STRING u8"Swift ✨"
#define OBJC_STRING @"Swift ✨"

#define INVALID_INTEGER_LITERAL_1 10_9
#define INVALID_INTEGER_LITERAL_2 10abc

// Check that macros that expand to macros from another module are imported
// correctly.
#define USES_MACRO_FROM_OTHER_MODULE_1 MACRO_FROM_IMPL
#define USES_MACRO_FROM_OTHER_MODULE_2 MACRO_FROM_PRIVATE_IMPL


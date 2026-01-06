#ifndef TEST_INTEROP_C_CHARS_INPUTS_IMPORT_CCHAR_TYPES_H
#define TEST_INTEROP_C_CHARS_INPUTS_IMPORT_CCHAR_TYPES_H

#include <stddef.h>

extern char a_char;
extern wchar_t a_wchar;

// This case is a regression test for a crash when importing constant initializers for wchar_t,
// since Unicode.Scalar cannot be initialized with integer literals.
const wchar_t an_initialized_wchar = 2;

#if __cplusplus
extern char8_t small_char;
#endif

#endif // TEST_INTEROP_C_CHARS_INPUTS_IMPORT_CCHAR_TYPES_H

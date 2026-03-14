// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

import macros

_ = INVALID_INTEGER_LITERAL_2

// Test that experimental diagnostics for the function like macro in the same header is suppressed.
// CHECK: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_INTEGER_LITERAL_2' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported
// CHECK-NOT: #define FUNC_LIKE_MACRO() 0
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_1' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_2' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_3' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_4' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_5' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_6' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: invalid arithmetic operand; only valid integers supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'INVALID_ARITHMETIC_7' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: operator '%' not supported in macro arithmetic
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'CHAR_LITERAL' unavailable (cannot import)
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: only numeric and string macro literals supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'UNSUPPORTED_1' not imported: structure not supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'UNSUPPORTED_2' not imported: structure not supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'UNSUPPORTED_3' not imported: structure not supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'UNSUPPORTED_4' not imported: structure not supported
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: note: macro 'UNSUPPORTED_5' not imported: structure not supported

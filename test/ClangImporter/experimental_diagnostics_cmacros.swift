// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s --strict-whitespace

import macros

_ = INVALID_INTEGER_LITERAL_2
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_INTEGER_LITERAL_2' in scope
// CHECK-NEXT: _ = INVALID_INTEGER_LITERAL_2
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'INVALID_INTEGER_LITERAL_2' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}        ^

_ = FUNC_LIKE_MACRO()
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'FUNC_LIKE_MACRO' in scope
// CHECK-NEXT: _ = FUNC_LIKE_MACRO()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'FUNC_LIKE_MACRO' unavailable: function like macros not supported
// CHECK-NEXT:      #define FUNC_LIKE_MACRO() 0
// CHECK-NEXT: {{^}}        ^
_ = FUNC_LIKE_MACRO()
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'FUNC_LIKE_MACRO' in scope
// CHECK-NEXT: _ = FUNC_LIKE_MACRO()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'FUNC_LIKE_MACRO' unavailable: function like macros not supported
// CHECK-NEXT:      #define FUNC_LIKE_MACRO() 0
// CHECK-NEXT: {{^}}        ^


_ = INVALID_ARITHMETIC_1
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_1' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_1
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_1' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_ARITHMETIC_1 5 + INVALID_INTEGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^

_ = INVALID_ARITHMETIC_2
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_2' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_2
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_2' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_ARITHMETIC_2 INVALID_INTEGER_LITERAL_1 + ADD_TWO
// CHECK-NEXT: {{^}}        ^

_ = INVALID_ARITHMETIC_3
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_3' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_3
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_3' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_ARITHMETIC_3 ADD_TWO + INVALID_INTEGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^

_ = INVALID_ARITHMETIC_4
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_4' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_4
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_4' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_ARITHMETIC_4                                                   \
// CHECK-NEXT: {{^}}        ^

_ = INVALID_ARITHMETIC_5
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_5' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_5
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_5' unavailable (cannot import)
// CHECK-NEXT:      #define INVALID_ARITHMETIC_5 1 + VERSION_STRING
// CHECK-NEXT: {{^}}        ^


UNSUPPORTED_4
// CHECK:      experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_4' in scope
// CHECK-NEXT: UNSUPPORTED_4
// CHECK-NEXT: ^~~~~~~~~~~~~
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_4' unavailable (cannot import)
// CHECK-NEXT:      #define UNSUPPORTED_4                                                          \
// CHECK-NEXT: {{^}}        ^

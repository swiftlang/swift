// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-clang-importer-diagnostics -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

import macros

_ = INVALID_INTEGER_LITERAL_2
// CHECK:      macros.h:{{[0-9]+}}:{{[0-9]+}}: error: invalid digit 'a' in decimal constant
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}                                    ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:9: error: macro 'INVALID_INTEGER_LITERAL_2' not imported
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:35: note: invalid numeric literal
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}                                  ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_INTEGER_LITERAL_2' in scope
// CHECK-NEXT: _ = INVALID_INTEGER_LITERAL_2
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~



_ = FUNC_LIKE_MACRO()
_ = FUNC_LIKE_MACRO()
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported
// CHECK-NEXT:      #define FUNC_LIKE_MACRO() 0
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'FUNC_LIKE_MACRO' in scope
// CHECK-NEXT: _ = FUNC_LIKE_MACRO()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'FUNC_LIKE_MACRO' in scope
// CHECK-NEXT: _ = FUNC_LIKE_MACRO()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~
// CHECK-NOT: macros.h:{{[0-9]+}}:{{[0-9]+}}: error: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported



_ = INVALID_ARITHMETIC_1
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_1' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_1 5 + INVALID_INTERGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_1' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_1
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_2
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_2' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_2 INVALID_INTEGER_LITERAL_1 + ADD_TWO
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_2' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_2
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_3
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_3' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_3 ADD_TWO + INVALID_INTEGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_3' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_3
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_4
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_4' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_4                                                   \
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_4' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_4
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_5
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_5' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_5 1 + VERSION_STRING
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_5' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_5
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_6
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_6' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_6 1 + 'c'
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_6' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_6
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~

_ = INVALID_ARITHMETIC_7
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'INVALID_ARITHMETIC_7' not imported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_7 3 % 2
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:32: note: operator '%' not supported in macro arithmetic
// CHECK-NEXT:      #define INVALID_ARITHMETIC_7 3 % 2
// CHECK-NEXT: {{^}}                               ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'INVALID_ARITHMETIC_7' in scope
// CHECK-NEXT: _ = INVALID_ARITHMETIC_7
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~



_ = CHAR_LITERAL
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'CHAR_LITERAL' not imported
// CHECK-NEXT:      #define CHAR_LITERAL 'a'
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:22: note: only numeric and string macro literals supported
// CHECK-NEXT:      #define CHAR_LITERAL 'a'
// CHECK-NEXT: {{^}}                     ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CHAR_LITERAL' in scope
// CHECK-NEXT: _ = CHAR_LITERAL
// CHECK-NEXT:     ^~~~~~~~~~~~



UNSUPPORTED_1
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'UNSUPPORTED_1' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_1 FUNC_LIKE_MACRO()
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_1' in scope
// CHECK-NEXT: UNSUPPORTED_1
// CHECK-NEXT: ^~~~~~~~~~~~~

UNSUPPORTED_2
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'UNSUPPORTED_2' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_2 FUNC_LIKE_MACRO_2(1)
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_2' in scope
// CHECK-NEXT: UNSUPPORTED_2
// CHECK-NEXT: ^~~~~~~~~~~~~

UNSUPPORTED_3
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'UNSUPPORTED_3' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_3 1 + FUNC_LIKE_MACRO_2(1)
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_3' in scope
// CHECK-NEXT: UNSUPPORTED_3
// CHECK-NEXT: ^~~~~~~~~~~~~

UNSUPPORTED_4
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'UNSUPPORTED_4' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_4                                                          \
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_4' in scope
// CHECK-NEXT: UNSUPPORTED_4
// CHECK-NEXT: ^~~~~~~~~~~~~

UNSUPPORTED_5
// CHECK:      macros.h:{{[0-9]+}}:9: error: macro 'UNSUPPORTED_5' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_5 1 + 1 + 1
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: experimental_diagnostics_cmacros.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'UNSUPPORTED_5' in scope
// CHECK-NEXT: UNSUPPORTED_5
// CHECK-NEXT: ^~~~~~~~~~~~~

// RUN: not %target-swift-frontend -enable-objc-interop -import-objc-header %S/Inputs/experimental_clang_importer_diagnostics_bridging_header.h -typecheck %s 2>&1 | %FileCheck %s

let s: PartialImport
s.c = 5
// CHECK:      experimental_clang_importer_diagnostics_bridging_header.swift:{{[0-9]+}}:3: error: value of type 'PartialImport' has no member 'c'
// CHECK-NEXT: s.c = 5
// CHECK-NEXT: ~ ^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:3: note: field 'c' not imported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^

_ = CFunctionReturningAForwardDeclaredInterface()
// CHECK:      experimental_clang_importer_diagnostics_bridging_header.swift:{{[0-9]+}}:5: error: cannot find 'CFunctionReturningAForwardDeclaredInterface' in scope
// CHECK-NEXT: _ = CFunctionReturningAForwardDeclaredInterface()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: function 'CFunctionReturningAForwardDeclaredInterface' not imported
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

_ = FUNC_LIKE_MACRO()
// CHECK:      experimental_clang_importer_diagnostics_bridging_header.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'FUNC_LIKE_MACRO' in scope
// CHECK-NEXT: _ = FUNC_LIKE_MACRO()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:9: note: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported
// CHECK-NEXT:      #define FUNC_LIKE_MACRO() 0
// CHECK-NEXT: {{^}}        ^

unsupported_return_type()
// CHECK:      experimental_clang_importer_diagnostics_bridging_header.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'unsupported_return_type' in scope
// CHECK-NEXT: unsupported_return_type()
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: function 'unsupported_return_type' not imported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: experimental_clang_importer_diagnostics_bridging_header.h:{{[0-9]+}}:1: note: built-in type 'Complex' not supported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^

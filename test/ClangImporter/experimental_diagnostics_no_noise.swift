// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -swift-version 5 -enable-objc-interop -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

// Referencing a single declaration should only surface diagnostics about that declaration, no more

import IncompleteTypes
import cfuncs
import ctypes

// CHECK-NOT: note
// CHECK-NOT: warning
// CHECK-NOT: error
let bar = Bar()
_ = bar.methodReturningForwardDeclaredInterface()
// CHECK:      experimental_diagnostics_no_noise.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Bar' has no member 'methodReturningForwardDeclaredInterface'
// CHECK-NEXT: _ = bar.methodReturningForwardDeclaredInterface()
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: method 'methodReturningForwardDeclaredInterface' unavailable (cannot import)
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

// CHECK-NOT: note
// CHECK-NOT: warning
// CHECK-NOT: error
unsupported_parameter_type(1,2)
// CHECK:      experimental_diagnostics_no_noise.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'unsupported_parameter_type' in scope
// CHECK-NEXT: unsupported_parameter_type(1,2)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'unsupported_parameter_type' unavailable (cannot import)
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param2' unavailable (cannot import)
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}                                           ^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:{{[0-9]+}}: note: built-in type 'Complex' not supported
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}                                           ^

// CHECK-NOT: note
// CHECK-NOT: warning
// CHECK-NOT: error
let s: PartialImport
s.c = 5
// CHECK:      experimental_diagnostics_no_noise.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'PartialImport' has no member 'c'
// CHECK-NEXT: s.c = 5
// CHECK-NEXT: ~ ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:{{[0-9]+}}: note: field 'c' unavailable (cannot import)
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:{{[0-9]+}}: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:{{[0-9]+}}: note: did you mean 'a'?
// CHECK-NEXT:     int a;
// CHECK-NEXT:         ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:{{[0-9]+}}: note: did you mean 'b'?
// CHECK-NEXT:     int b;
// CHECK-NEXT:         ^

// CHECK-NOT: note
// CHECK-NOT: warning
// CHECK-NOT: error

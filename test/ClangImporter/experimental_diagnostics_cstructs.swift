// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s --strict-whitespace

import ctypes

let s: PartialImport
s.c = 5
// CHECK:      experimental_diagnostics_cstructs.swift:{{[0-9]+}}:3: error: value of type 'PartialImport' has no member 'c'
// CHECK-NEXT: s.c = 5
// CHECK-NEXT: ~ ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: field 'c' unavailable (cannot import)
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^

partialImport.c = 5
// CHECK:      experimental_diagnostics_cstructs.swift:{{[0-9]+}}:15: error: value of type 'PartialImport' has no member 'c'
// CHECK-NEXT: partialImport.c = 5
// CHECK-NEXT: ~~~~~~~~~~~~~ ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: field 'c' unavailable (cannot import)
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^

var newPartialImport = PartialImport()
newPartialImport.a = 5
newPartialImport.b = 5
newPartialImport.d = 5
// CHECK:      experimental_diagnostics_cstructs.swift:{{[0-9]+}}:18: error: value of type 'PartialImport' has no member 'd'
// CHECK-NEXT: newPartialImport.d = 5
// CHECK-NEXT: ~~~~~~~~~~~~~~~~ ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: field 'd' unavailable (cannot import)
// CHECK-NEXT:   int _Complex d;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex d;
// CHECK-NEXT:   ^

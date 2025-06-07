// RUN: not %target-swift-frontend -I %S/Inputs/custom-modules -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s --strict-whitespace 

// This test tests that requests for diagnosis, performed by name,
// resolve to the correct Clang declarations, even when the same name is used
// in multiple contexts.

import CommonName

// Free C function
commonName()
// CHECK:      experimental_diagnostics_lookup_accuracy.swift:10:1: error: cannot find 'commonName' in scope
// CHECK-NEXT: commonName()
// CHECK-NEXT: ^~~~~~~~~~
// CHECK-NEXT: CommonName.h:5:1: note: function 'commonName' unavailable (cannot import)
// CHECK-NEXT:      _Complex int commonName();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: CommonName.h:5:1: note: return type unavailable (cannot import)
// CHECK-NEXT:      _Complex int commonName();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: CommonName.h:5:1: note: built-in type 'Complex' not supported
// CHECK-NEXT:      _Complex int commonName();
// CHECK-NEXT: {{^}}^

// C struct field
let s: MyStruct
s.commonName = 5
// CHECK:      experimental_diagnostics_lookup_accuracy.swift:26:3: error: value of type 'MyStruct' has no member 'commonName'
// CHECK-NEXT: s.commonName = 5
// CHECK-NEXT: ~ ^
// CHECK-NEXT: CommonName.h:2:3: note: field 'commonName' unavailable (cannot import)
// CHECK-NEXT:   int _Complex commonName;
// CHECK-NEXT:   ^
// CHECK-NEXT: CommonName.h:2:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex commonName;
// CHECK-NEXT:   ^

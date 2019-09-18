// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Setup builds a parseable interface for a module SomeModule (built from some-module.swift).
// This test checks we still build and load its corresponding .swiftmodule when the file that imports it contains an error prior to the import statement.

// Setup phase 1: Write the input file.
//
// RUN: echo 'public func SomeFunc() -> Int { return 42; }' >>%t/some-module.swift

// Setup phase 2: build the module.
//
// RUN: %target-swift-frontend -I %t -emit-module-interface-path %t/SomeModule.swiftinterface -module-name SomeModule %t/some-module.swift -emit-module -o /dev/null

// Actual test: compile and verify the import succeeds (i.e. we only report the error in this file)
//
// RUN: %target-swift-frontend -typecheck -verify -I %t -module-cache-path %t/modulecache %s

unresolved // expected-error {{use of unresolved identifier 'unresolved'}}

import SomeModule

print(SomeFunc())

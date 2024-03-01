// RUN: %empty-directory(%t)

// RUN: touch %t/empty.swift
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -verify -emit-module -o %t/errors.partial.swiftmodule -module-name errors -experimental-allow-module-with-compiler-errors %t/errors.swift
// RUN: %target-swift-frontend -emit-module -o %t/errorsempty.partial.swiftmodule -module-name errors %t/empty.swift

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/errors.partial.swiftmodule %t/errorsempty.partial.swiftmodule

// RUN: %target-swift-frontend -emit-module -o %t/mods/uses.swiftmodule -experimental-allow-module-with-compiler-errors -I %t/mods %t/uses.swift -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=CHECK-USES %s

// BEGIN errors.swift
typealias AnAlias = undefined // expected-error {{cannot find type 'undefined'}}

// BEGIN uses.swift
import errors
func test(a: AnAlias) {}
// CHECK-USES-NOT: cannot find type 'AnAlias' in scope

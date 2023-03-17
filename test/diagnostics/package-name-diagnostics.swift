// RUN: %empty-directory(%t)

// Package name should have valid characters
// RUN: not --crash %target-swift-frontend -module-name Logging -package-name My-Logging%Pkg %s -emit-module -emit-module-path %t/Logging.swiftmodule 2> %t/resultA.output
// RUN: %FileCheck %s -input-file %t/resultA.output -check-prefix CHECK-BAD
// CHECK-BAD: package name "My-Logging%Pkg" is not a valid identifier
// CHECK-BAD: decl has a package access level but no -package-name was passed
// CHECK-BAD: non-public decl has no formal access scope

// Package name should not be empty
// RUN: not --crash %target-swift-frontend -typecheck %s -package-name "" 2>&1 | %FileCheck %s -check-prefix CHECK-EMPTY
// CHECK-EMPTY: package name "" is not a valid identifier
// CHECK-EMPTY: decl has a package access level but no -package-name was passed
// CHECK-EMPTY: non-public decl has no formal access scope

// If package access level is used but no package-name is passed, it should error
// RUN: not --crash %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s -check-prefix CHECK-MISSING
// CHECK-MISSING: decl has a package access level but no -package-name was passed
// CHECK-MISSING: non-public decl has no formal access scope

// Package name can be same as the module name
// RUN: %target-swift-frontend -module-name Logging -package-name Logging %s -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: test -f %t/Logging.swiftmodule

// Package name can be a standard library name
// RUN: %target-swift-frontend -module-name Logging -package-name Swift %s -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: test -f %t/Logging.swiftmodule

package func log() {}


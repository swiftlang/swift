// RUN: %empty-directory(%t)

// Package name should have valid characters
// RUN: not %target-swift-frontend -module-name Logging -package-name My-Logging%Pkg %s -emit-module -emit-module-path %t/Logging.swiftmodule 2>&1 | %FileCheck %s -check-prefix CHECK-BAD
// CHECK-BAD: package name "My-Logging%Pkg" is not a valid identifier

// Package name cannot be a standard library name
// RUN: not %target-swift-frontend -module-name Logging -package-name Swift %s -emit-module -emit-module-path %t/Logging.swiftmodule 2>&1 | %FileCheck %s -check-prefix CHECK-STDLIB
// CHECK-STDLIB: package name "Swift" is reserved for the standard library

// Package name can be same as the module name
// RUN: %target-swift-frontend -module-name Logging -package-name Logging %s -emit-module -emit-module-path %t/Logging.swiftmodule
// RUN: test -f %t/Logging.swiftmodule

public func log() {}


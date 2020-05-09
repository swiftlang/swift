// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/../Inputs/empty.swift

// Make sure we don't get any errors when importing the modules that were compiled for testing
// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules %s -verify

// Make sure no errors are thrown if we disable the @testable check
// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules %s -verify -disable-testable-attr-requires-testable-module

// Make sure we get the one fatal error for the stdlib not being compiled for testing
// RUN: not %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules %s 2>&1 -DINCLUDE_STDLIB | %FileCheck %s


#if INCLUDE_STDLIB
// CHECK: module 'Swift' was not compiled for testing
@testable import Swift
#endif

// CHECK-NOT: module 'empty' was not compiled for testing
@testable import empty // no-error
// CHECK-NOT: module 'Testable_ClangModule' was not compiled for testing
@testable import Testable_ClangModule // no-error

_ = clangGlobal

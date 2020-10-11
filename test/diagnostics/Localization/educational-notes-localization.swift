// RUN: not %target-swift-frontend -no-color-diagnostics -print-educational-notes -diagnostic-documentation-path %S/Inputs/test-docs/ -locale en -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK-EN
// RUN: not %target-swift-frontend -no-color-diagnostics -print-educational-notes -diagnostic-documentation-path %S/Inputs/test-docs/ -locale test -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK-TEST

// Test a note with an available translation.
@dynamicCallable struct Foo {}
// CHECK-EN: error: @dynamicCallable attribute requires 'Foo' to have either a valid 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method
// CHECK-EN: @dynamicCallable note contents - en

// CHECK-TEST: error: @dynamicCallable attribute requires 'Foo' to have either a valid 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method
// CHECK-TEST: @dynamicCallable note contents - test

// Test a note without an available translation.
@propertyWrapper struct Bar {}
// CHECK-EN: error: property wrapper type 'Bar' does not contain a non-static property named 'wrappedValue'
// CHECK-EN: @propertyWrapper note contents - en

// CHECK-TEST: error: property wrapper type 'Bar' does not contain a non-static property named 'wrappedValue'
// CHECK-TEST: @propertyWrapper note contents - en

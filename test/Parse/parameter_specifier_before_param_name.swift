// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
actor MyActor {}

class MyClass {}

// Lifetime specifiers before parameter names were disallowed in Swift 3 (SE-0031).
// `isolated`, `sending` and `_const` got added after Swift 3 without a diagnostic 
// to disallow them before parameter names.

func foo(inout x b: MyClass) {} // expected-cparser-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} expected-astgen-error {{'inout' before a parameter name is not allowed}}

func foo(borrowing x b: MyClass) {} // expected-cparser-error {{'borrowing' before a parameter name is not allowed, place it before the parameter type instead}} expected-astgen-error {{'borrowing' before a parameter name is not allowed}}

@available(SwiftStdlib 5.1, *)
func foo(isolated x b: MyActor) {} // expected-warning {{'isolated' before a parameter name is not allowed, place it before the parameter type instead; this is an error in the Swift 6 language mode}}

func foo(_const x b: MyClass) {} // expected-warning {{'_const' before a parameter name is not allowed, place it before the parameter type instead; this is an error in the Swift 6 language mode}}

@available(SwiftStdlib 5.1, *)
func foo(sending x b: MyActor) {} // expected-cparser-warning {{'sending' before a parameter name is not allowed, place it before the parameter type instead}} expected-astgen-error {{'sending' before a parameter name is not allowed}}

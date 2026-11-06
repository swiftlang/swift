// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: cat %s | grep -v _expose > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Functions -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/header.h
// RUN: %FileCheck %s < %t/header.h

// RUN: %check-interop-cxx-header-in-clang(%t/header.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

// The C++ bindings for throwing functions only understand untyped `throws`,
// whose error is returned as an `any Error` box in the error slot. Typed
// throws (SE-0413) and `throws(Never)` use a different ABI, so they cannot be
// exposed to C++ yet, even with GenerateBindingsForThrowingFunctionsInCXX.

public enum MyError: Error {
    case fail
    case worse
}

@_expose(Cxx)
public func untypedThrowsFunction() throws { }

@_expose(Cxx)
public func anyErrorThrowsFunction() throws(any Error) { }

@_expose(Cxx) // expected-error {{global function 'typedThrowsFunction()' can not yet be represented in C++ as it may throw an error}}
public func typedThrowsFunction() throws(MyError) { }

@_expose(Cxx) // expected-error {{global function 'typedThrowsFunctionWithReturn()' can not yet be represented in C++ as it may throw an error}}
public func typedThrowsFunctionWithReturn() throws(MyError) -> Int { return 0 }

@_expose(Cxx) // expected-error {{global function 'neverThrowsFunction()' can not yet be represented in C++ as it may throw an error}}
public func neverThrowsFunction() throws(Never) { }

@_expose(Cxx)
public struct HasTypedThrowsMembers {
    public let stored: Int = 0

    @_expose(Cxx)
    public func untypedThrowsMethod() throws { }

    @_expose(Cxx) // expected-error {{instance method 'typedThrowsMethod()' can not yet be represented in C++ as it may throw an error}}
    public func typedThrowsMethod() throws(MyError) { }

    @_expose(Cxx) // expected-error {{initializer 'init(checked:)' can not yet be represented in C++ as it may throw an error}}
    public init(checked: Int) throws(MyError) { }

    @_expose(Cxx) // expected-error {{property 'computed' can not yet be represented in C++ as it may throw an error}}
    public var computed: Int {
        get throws(MyError) { 42 }
    }

    // Subscripts cannot be marked with the expose attribute, so only the
    // header checks below cover this one.
    public subscript(index: Int) -> Int {
        get throws(MyError) { index }
    }
}

// CHECK: class SWIFT_SYMBOL("s:9Functions21HasTypedThrowsMembersV") HasTypedThrowsMembers final {
// CHECK-NOT: {{ }}typedThrowsMethod(
// CHECK-NOT: init(
// CHECK-NOT: getComputed(
// CHECK-NOT: operator [](
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> untypedThrowsMethod() const SWIFT_SYMBOL("s:9Functions21HasTypedThrowsMembersV07untypedD6MethodyyKF");
// CHECK-NOT: {{ }}typedThrowsMethod(
// CHECK-NOT: init(
// CHECK-NOT: getComputed(
// CHECK-NOT: operator [](
// CHECK: };

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> anyErrorThrowsFunction() SWIFT_SYMBOL("s:9Functions22anyErrorThrowsFunctionyyKF") {
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> untypedThrowsFunction() SWIFT_SYMBOL("s:9Functions21untypedThrowsFunctionyyKF") {
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> HasTypedThrowsMembers::untypedThrowsMethod() const {
// CHECK-NOT: HasTypedThrowsMembers::typedThrowsMethod(
// CHECK-NOT: HasTypedThrowsMembers::init(
// CHECK-NOT: HasTypedThrowsMembers::getComputed(
// CHECK-NOT: HasTypedThrowsMembers::operator [](

// CHECK: // Unavailable in C++: Swift global function 'neverThrowsFunction()'. {{.*}}can not yet be represented in C++ as it may throw an error.
// CHECK: // Unavailable in C++: Swift global function 'typedThrowsFunction()'. {{.*}}can not yet be represented in C++ as it may throw an error.
// CHECK: // Unavailable in C++: Swift global function 'typedThrowsFunctionWithReturn()'. {{.*}}can not yet be represented in C++ as it may throw an error.

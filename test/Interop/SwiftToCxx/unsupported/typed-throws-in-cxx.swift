// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: cat %s | grep -v _expose > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Functions -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/header.h
// RUN: %FileCheck %s < %t/header.h

// RUN: %check-interop-cxx-header-in-clang(%t/header.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

// Typed throws (SE-0413) uses a different ABI than untyped throws: the
// thrown error is not returned as an `any Error` box in the error slot.
// Such functions cannot be exposed to C++ yet, even with
// GenerateBindingsForThrowingFunctionsInCXX enabled.

public enum MyError: Error {
    case fail
    case worse
}

@_expose(Cxx) // expected-error {{global function 'typedThrowsFunction()' can not yet be represented in C++ as it may throw an error}}
public func typedThrowsFunction() throws(MyError) { }

@_expose(Cxx) // expected-error {{global function 'typedThrowsFunctionWithReturn()' can not yet be represented in C++ as it may throw an error}}
public func typedThrowsFunctionWithReturn() throws(MyError) -> Int { return 0 }

@_expose(Cxx)
public struct HasTypedThrowsMembers {
    public let stored: Int = 0

    @_expose(Cxx) // expected-error {{instance method 'typedThrowsMethod()' can not yet be represented in C++ as it may throw an error}}
    public func typedThrowsMethod() throws(MyError) { }

    @_expose(Cxx) // expected-error {{initializer 'init(checked:)' can not yet be represented in C++ as it may throw an error}}
    public init(checked: Int) throws(MyError) { }

    public var computed: Int {
        get throws(MyError) { 42 }
    }
}

// No throwing thunk may be emitted anywhere in the header for these decls.
// CHECK-NOT: SWIFT_INLINE_THUNK swift::ThrowingResult

// CHECK: // Unavailable in C++: Swift global function 'typedThrowsFunction()'.
// CHECK: // Unavailable in C++: Swift global function 'typedThrowsFunctionWithReturn()'.

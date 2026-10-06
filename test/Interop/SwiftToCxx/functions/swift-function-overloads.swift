// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct HasOverloadedMethods {
    let x: Int

    public func overloadedMethod(x _: Int) { }
    public func overloadedMethod(y _: Int) { }
}

// CHECK: class SWIFT_SYMBOL("s:9Functions20HasOverloadedMethodsV") HasOverloadedMethods final {
// CHECK:        SWIFT_INLINE_THUNK void overloadedMethod(swift::Int _1) const noexcept SWIFT_SYMBOL("s:9Functions20HasOverloadedMethodsV16overloadedMethod1xySi_tF");
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'overloadedMethod(y:)'. An overload with the same C++ parameter types already exists.
// CHECK-NEXT: private:

// Different arity should always work.
public func arityOverload() { }
public func arityOverload(_ x: Int) { }

// CHECK: void arityOverload() noexcept
// CHECK: void arityOverload(swift::Int x) noexcept

// Same-arity overloads with different C++ types should both be emitted.
public func overloadedFunc(_ x: Int) { }
public func overloadedFunc(_ y: Float) { }

public func overloadedFuncArgLabel(x _: Int) { }
public func overloadedFuncArgLabel(y _: Int) { }

// CHECK: void overloadedFunc(swift::Int x) noexcept
// CHECK: void overloadedFunc(float y) noexcept

// CHECK: // Unavailable in C++: Swift global function 'overloadedFuncArgLabel(y:)'. An overload with the same C++ parameter types already exists.


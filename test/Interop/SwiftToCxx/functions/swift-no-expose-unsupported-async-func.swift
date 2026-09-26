// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -target %target-swift-5.1-abi-triple -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-NOT: SWIFT_EXTERN double $s9Functions9asyncFuncyS2dYaF(double x) SWIFT_NOEXCEPT SWIFT_CALL; // asyncFunc(_:)

// CHECK: class SWIFT_SYMBOL("s:9Functions20ClassWithAsyncMethodC") ClassWithAsyncMethod final : public swift::_impl::RefCountedClass {
// CHECK:        static SWIFT_INLINE_THUNK ClassWithAsyncMethod init() noexcept SWIFT_SYMBOL("s:9Functions20ClassWithAsyncMethodCACycfc");
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'asyncMethod(_:)'. async instance method 'asyncMethod' can not be exposed to C++.
// CHECK-NEXT:   SWIFT_INLINE_THUNK void syncMethod() noexcept SWIFT_SYMBOL("s:9Functions20ClassWithAsyncMethodC04syncE0yyF");
// CHECK-NEXT: protected:

// CHECK: class SWIFT_SYMBOL("s:9Functions21StructWithAsyncMethodV") StructWithAsyncMethod final {
// CHECK:        return *this;
// CHECK-NEXT:   }
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'asyncMethod(_:)'. async instance method 'asyncMethod' can not be exposed to C++.
// CHECK-NEXT: private:

// CHECK: // Unavailable in C++: Swift global function 'asyncFunc(_:)'.{{.*}}can not be exposed to C++.
// CHECK-EMPTY:
// CHECK-NEXT:  } // namespace Functions

// REQUIRES: concurrency

public func asyncFunc(_ x: Double) async -> Double { return 2 * x }

public final class ClassWithAsyncMethod {
    public init() {}
    public func asyncMethod(_ x: Double) async -> Double { return 2 * x }
    public func syncMethod() {}
}

public struct StructWithAsyncMethod {
    let x: Double
    public func asyncMethod(_ x: Double) async -> Double { return 2 * x }
}

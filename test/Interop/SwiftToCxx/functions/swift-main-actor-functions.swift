// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -target %target-swift-5.1-abi-triple -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR)

// The macro also works when another module's header is included before it is
// defined.
// RUN: echo 'public func otherModuleFunc() {}' > %t/other.swift
// RUN: %target-swift-frontend %t/other.swift -module-name Other -clang-header-expose-decls=all-public -typecheck -emit-clang-header-path %t/other.h
// RUN: echo '#include "other.h"' > %t/include-order.h
// RUN: echo '#define SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR' >> %t/include-order.h
// RUN: cat %t/functions.h >> %t/include-order.h
// RUN: %check-interop-cxx-header-in-clang(%t/include-order.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: concurrency

// CHECK-LABEL: namespace _impl {
// CHECK:       #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  SWIFT_EXTERN int $s9Functions13mainActorFuncys5Int32VADF(int x) SWIFT_NOEXCEPT SWIFT_CALL; // mainActorFunc(_:)
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  SWIFT_EXTERN void $s9Functions16mainActorGenericyyxlF({{.*}}) SWIFT_NOEXCEPT SWIFT_CALL; // mainActorGeneric(_:)
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  SWIFT_EXTERN void $s9Functions13nonisolatedFnyyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // nonisolatedFn()

// CHECK:       #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK int mainActorFunc(int x) noexcept SWIFT_SYMBOL("s:9Functions13mainActorFuncys5Int32VADF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:    swift::_impl::checkMainActorIsolation();
// CHECK-NEXT:    return Functions::_impl::$s9Functions13mainActorFuncys5Int32VADF(x);
// CHECK-NEXT:  }
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-EMPTY:
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  template<class T_0_0>
// CHECK-NEXT:  #ifdef __cpp_concepts
// CHECK-NEXT:  requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT:  #endif // __cpp_concepts
// CHECK-NEXT:  SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK void mainActorGeneric(const T_0_0& x) noexcept SWIFT_SYMBOL("s:9Functions16mainActorGenericyyxlF") {
// CHECK:         swift::_impl::checkMainActorIsolation();
// CHECK-NEXT:    Functions::_impl::$s9Functions16mainActorGenericyyxlF(
// CHECK-NEXT:  }
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-EMPTY:
// CHECK-NEXT:  SWIFT_INLINE_THUNK void nonisolatedFn() noexcept SWIFT_SYMBOL("s:9Functions13nonisolatedFnyyF") {
// CHECK-NEXT:    Functions::_impl::$s9Functions13nonisolatedFnyyF();
// CHECK-NEXT:  }

// CHECK: // Unavailable in C++: Swift global function 'mainActorAsync()'. async global function 'mainActorAsync()' can not be exposed to C++.
// CHECK: // Unavailable in C++: Swift global function 'otherGlobalActorFunc()'. actor-isolated global function 'otherGlobalActorFunc()' can not be exposed to C++.

@MainActor public func mainActorFunc(_ x: CInt) -> CInt { return x }

@MainActor public func mainActorGeneric<T>(_ x: T) {}

public func nonisolatedFn() {}

@MainActor public func mainActorAsync() async {}

@globalActor public actor OtherActor {
  public static let shared = OtherActor()
}

@OtherActor public func otherGlobalActorFunc() {}

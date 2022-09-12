// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxxTy -emit-clang-header-path %t/UseCxxTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-public-decls

// RUN: %FileCheck %s < %t/UseCxxTy.h

// FIXME: remove once https://github.com/apple/swift/pull/60971 lands.
// RUN: echo "#include \"header.h\"" > %t/full-cxx-swift-cxx-bridging.h
// RUN: cat %t/UseCxxTy.h >> %t/full-cxx-swift-cxx-bridging.h

// RUN: %check-interop-cxx-header-in-clang(%t/full-cxx-swift-cxx-bridging.h -Wno-reserved-identifier)

// FIXME: test in C++ with modules (but libc++ modularization is preventing this)

//--- header.h

struct Trivial {
    short x, y;
};

namespace ns {

    struct TrivialinNS {
        short x, y;
    };

    template<class T>
    struct NonTrivialTemplate {
        T x;

        NonTrivialTemplate();
        NonTrivialTemplate(const NonTrivialTemplate<T> &) = default;
        NonTrivialTemplate(NonTrivialTemplate<T> &&) = default;
        ~NonTrivialTemplate() {}
    };

    using TypeAlias = NonTrivialTemplate<TrivialinNS>;

    struct NonTrivialImplicitMove {
        NonTrivialTemplate<int> member;
    };
}

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func retNonTrivial() -> ns.NonTrivialTemplate<CInt> {
    return ns.NonTrivialTemplate<CInt>()
}

public func retNonTrivial2() -> ns.NonTrivialTemplate<ns.TrivialinNS> {
    return ns.NonTrivialTemplate<ns.TrivialinNS>()
}

public func retNonTrivialImplicitMove() -> ns.NonTrivialImplicitMove {
    return ns.NonTrivialImplicitMove()
}

public func retNonTrivialTypeAlias() -> ns.TypeAlias {
    return ns.TypeAlias()
}

public func retTrivial() -> Trivial {
    return Trivial()
}

public func takeNonTrivial2(_ x: ns.NonTrivialTemplate<ns.TrivialinNS>) {
}

public func takeTrivial(_ x: Trivial) {
}

public func takeTrivialInout(_ x: inout Trivial) {
}

// CHECK: #if __has_feature(objc_modules)
// CHECK: #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT:#endif
// CHECK-NEXT: #pragma clang module import CxxTest;
// CHECK-NEXT: #endif


// CHECK: SWIFT_EXTERN void $s8UseCxxTy13retNonTrivialSo2nsO02__b18TemplateInstN2ns18efH4IiEEVyF(SWIFT_INDIRECT_RESULT void * _Nonnull) SWIFT_NOEXCEPT SWIFT_CALL; // retNonTrivial()
// CHECK: SWIFT_EXTERN struct swift_interop_returnStub_UseCxxTy_uint32_t_0_4 $s8UseCxxTy10retTrivialSo0E0VyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // retTrivial()

// CHECK: inline ns::NonTrivialTemplate<int> retNonTrivial() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(ns::NonTrivialTemplate<int>)) char storage[sizeof(ns::NonTrivialTemplate<int>)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<ns::NonTrivialTemplate<int> *>(storage);
// CHECK-NEXT: _impl::$s8UseCxxTy13retNonTrivialSo2nsO02__b18TemplateInstN2ns18efH4IiEEVyF(storage);
// CHECK-NEXT: ns::NonTrivialTemplate<int> result(std::move(*storageObjectPtr));
// CHECK-NEXT: storageObjectPtr->~NonTrivialTemplate();
// CHECK-NEXT: return result;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK: inline ns::NonTrivialTemplate<ns::TrivialinNS> retNonTrivial2() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(ns::NonTrivialTemplate<ns::TrivialinNS>)) char storage[sizeof(ns::NonTrivialTemplate<ns::TrivialinNS>)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<ns::NonTrivialTemplate<ns::TrivialinNS> *>(storage);
// CHECK-NEXT: _impl::$s8UseCxxTy14retNonTrivial2So2nsO02__b18TemplateInstN2ns18e7TrivialH20INS_11TrivialinNSEEEVyF(storage);
// CHECK-NEXT: ns::NonTrivialTemplate<ns::TrivialinNS> result(std::move(*storageObjectPtr));
// CHECK-NEXT: storageObjectPtr->~NonTrivialTemplate();
// CHECK-NEXT: return result;
// CHECK-NEXT: }

// CHECK: inline ns::NonTrivialImplicitMove retNonTrivialImplicitMove() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(ns::NonTrivialImplicitMove)) char storage[sizeof(ns::NonTrivialImplicitMove)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<ns::NonTrivialImplicitMove *>(storage);
// CHECK-NEXT: _impl::$s8UseCxxTy25retNonTrivialImplicitMoveSo2nsO0efgH0VyF(storage);
// CHECK-NEXT: ns::NonTrivialImplicitMove result(std::move(*storageObjectPtr));
// CHECK-NEXT: storageObjectPtr->~NonTrivialImplicitMove();
// CHECK-NEXT: return result;
// CHECK-NEXT: }

// CHECK: ns::NonTrivialTemplate<ns::TrivialinNS> retNonTrivialTypeAlias() noexcept SWIFT_WARN_UNUSED_RESULT {

// CHECK: inline Trivial retTrivial() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: alignas(alignof(Trivial)) char storage[sizeof(Trivial)];
// CHECK-NEXT: auto * _Nonnull storageObjectPtr = reinterpret_cast<Trivial *>(storage);
// CHECK-NEXT: _impl::swift_interop_returnDirect_UseCxxTy_uint32_t_0_4(storage, _impl::$s8UseCxxTy10retTrivialSo0E0VyF());
// CHECK-NEXT: return *storageObjectPtr;
// CHECK-NEXT: }

// CHECK: inline void takeNonTrivial2(const ns::NonTrivialTemplate<ns::TrivialinNS>& x) noexcept {
// CHECK-NEXT:   return _impl::$s8UseCxxTy15takeNonTrivial2yySo2nsO02__b18TemplateInstN2ns18e7TrivialH20INS_11TrivialinNSEEEVF(swift::_impl::getOpaquePointer(x));
// CHECK-NEXT: }

// CHECK: inline void takeTrivial(const Trivial& x) noexcept {
// CHECK-NEXT:   return _impl::$s8UseCxxTy11takeTrivialyySo0E0VF(_impl::swift_interop_passDirect_UseCxxTy_uint32_t_0_4(reinterpret_cast<const char *>(swift::_impl::getOpaquePointer(x))));
// CHECK-NEXT: }

// CHECK: inline void takeTrivialInout(Trivial& x) noexcept {
// CHECK-NEXT:   return _impl::$s8UseCxxTy16takeTrivialInoutyySo0E0VzF(swift::_impl::getOpaquePointer(x));
// CHECK-NEXT: }

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

public struct ComesFirst {
    let x: Int

    public func returnsLaterOpt() -> LaterGeneric<ComesFirst> { return LaterGeneric(x: ComesFirst(x: 0)) }
}

public struct LaterGeneric<T> {
    let x: T
}

// CHECK: static inline const constexpr bool isUsableInGenericContext<Generics::ComesFirst> = true;
// CHECK: class ComesFirst final {
// CHECK: LaterGeneric<ComesFirst> returnsLaterOpt() const;
// CHECK: class LaterGeneric final {

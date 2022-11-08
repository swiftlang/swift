// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Generics -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/generics.h)

@_expose(Cxx)
public enum ComesFirstEnum {
    case A
    case B

    public func returnsLaterOpt() -> LaterGeneric<ComesFirstEnum> { return LaterGeneric(x: ComesFirstEnum.A) }

    public var first: ComesFirstStruct {
        return ComesFirstStruct(x: 42)
    }
}

@_expose(Cxx)
public struct ComesFirstStruct {
    let x: Int

    public func returnsLaterOpt() -> LaterGeneric<ComesFirstStruct> { return LaterGeneric(x: ComesFirstStruct(x: 0)) }
}

@_expose(Cxx)
public struct LaterGeneric<T> {
    let x: T
}


// CHECK: class LaterGeneric;

// CHECK: class ComesFirstStruct;
// CHECK: static inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstStruct> = true;

// CHECK: class ComesFirstEnum;
// CHECK: static inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstEnum> = true;

// CHECK: class ComesFirstEnum final {
// CHECK: LaterGeneric<ComesFirstEnum> returnsLaterOpt() const;

// CHECK: namespace Generics __attribute__((swift_private)) {
// CHECK-EMPTY:
// CHECK-NEXT:  namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT:  class _impl_ComesFirstStruct;

// CHECK: class ComesFirstStruct final {
// CHECK: LaterGeneric<ComesFirstStruct> returnsLaterOpt() const;
// CHECK: class LaterGeneric final {

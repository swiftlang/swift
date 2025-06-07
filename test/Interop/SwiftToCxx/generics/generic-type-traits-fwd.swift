// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Generics -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/generics.h
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


// CHECK: class SWIFT_SYMBOL("s:8Generics12LaterGenericV") LaterGeneric;

// CHECK: class SWIFT_SYMBOL("s:8Generics16ComesFirstStructV") ComesFirstStruct;
// CHECK: inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstStruct> = true;

// CHECK: class SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO") ComesFirstEnum;
// CHECK: inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstEnum> = true;

// CHECK: class SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO") ComesFirstEnum final {
// CHECK: LaterGeneric<ComesFirstEnum> returnsLaterOpt() const SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO15returnsLaterOptAA0F7GenericVyACGyF");

// CHECK: namespace Generics SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Generics") {
// CHECK-EMPTY:
// CHECK-NEXT:  namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT:  class _impl_ComesFirstStruct;

// CHECK: class SWIFT_SYMBOL("s:8Generics16ComesFirstStructV") ComesFirstStruct final {
// CHECK: LaterGeneric<ComesFirstStruct> returnsLaterOpt() const SWIFT_SYMBOL("s:8Generics16ComesFirstStructV15returnsLaterOptAA0F7GenericVyACGyF");
// CHECK: class SWIFT_SYMBOL("s:8Generics12LaterGenericV") LaterGeneric final {

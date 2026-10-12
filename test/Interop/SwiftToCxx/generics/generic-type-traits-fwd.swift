// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Generics -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/generics.h)

@_expose(Cxx)
public final class ComesFirstClass {
    public init() {}

    public init(_ x: LaterGeneric<Int>) {}

    public func returnsLaterStruct() -> LaterGeneric<ComesFirstClass> { return LaterGeneric(x: self) }

    public func takesLaterStruct(_ x: LaterGeneric<Int>) {}

    public func returnsLaterEnum() -> LaterGenericEnum<Int> { return .empty }

    public func takesLaterEnum(_ x: LaterGenericEnum<Int>) {}

    public var laterStruct: LaterGeneric<Int> {
        get { return LaterGeneric(x: 0) }
        set {}
    }
}

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

@_expose(Cxx)
public enum LaterGenericEnum<T> {
    case value(T)
    case empty

    public func returnsFirstClass() -> ComesFirstClass { return ComesFirstClass() }
}

// Printed after LaterGeneric and LaterGenericEnum.
@_expose(Cxx)
public class LaterGenericUser {
    public func returnsLaterStruct() -> LaterGeneric<LaterGenericUser> { return LaterGeneric(x: self) }

    public func takesLaterEnum(_ x: LaterGenericEnum<Int>) {}
}

@_expose(Cxx)
public final class GenericMemberCollision {
    public init() {}
    public var item: LaterGeneric<Int> { LaterGeneric(x: 1) }
    public func getItem() -> Int { return 2 }

    public func value() -> LaterGeneric<Int> { return LaterGeneric(x: 3) }
    public func value() -> Int { return 4 }

    public static func shared() -> LaterGeneric<Int> { return LaterGeneric(x: 5) }
    public func shared() -> Int { return 6 }
}

@_expose(Cxx)
public final class GenericMemberCollisionReversed {
    public init() {}
    public func getItem() -> Int { return 2 }
    public var item: LaterGeneric<Int> { LaterGeneric(x: 1) }

    public func value() -> Int { return 4 }
    public func value() -> LaterGeneric<Int> { return LaterGeneric(x: 3) }

    public func shared() -> Int { return 6 }
    public static func shared() -> LaterGeneric<Int> { return LaterGeneric(x: 5) }
}

@_expose(Cxx)
public class InheritedBase {
    public init() {}
    public func getItem() -> Int { return 8 }
    public func consume(_ value: Int) -> Int { return value + 10 }
    public subscript(_ value: Int) -> Int { return value + 12 }
}

@_expose(Cxx)
public class InheritedMiddle: InheritedBase {}

@_expose(Cxx)
public final class InheritedUser: InheritedMiddle {
    public override init() { super.init() }
    public var item: LaterGeneric<Int> { LaterGeneric(x: 9) }
    public func consume(_ value: LaterGeneric<Int>) -> Int { return value.x }
    public subscript(_ value: LaterGeneric<Int>) -> Int { return value.x }
    public func novel() -> LaterGeneric<Int> { return LaterGeneric(x: 11) }
    public func read(_ value: LaterGeneric<Int>) -> Int { return value.x }
}


// CHECK: class SWIFT_SYMBOL("s:8Generics12LaterGenericV") LaterGeneric;
// CHECK: class SWIFT_SYMBOL("s:8Generics16LaterGenericEnumO") LaterGenericEnum;

// CHECK: class SWIFT_SYMBOL("s:8Generics15ComesFirstClassC") ComesFirstClass final : public swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT:   static SWIFT_INLINE_THUNK ComesFirstClass init() noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassCACycfc");
// CHECK-NEXT:   static SWIFT_INLINE_THUNK ComesFirstClass init(const LaterGeneric<swift::Int>& x) noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassCyAcA12LaterGenericVySiGcfc");
// CHECK-NEXT:   SWIFT_INLINE_THUNK LaterGeneric<ComesFirstClass> returnsLaterStruct() noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC18returnsLaterStructAA0F7GenericVyACGyF");
// CHECK-NEXT:   SWIFT_INLINE_THUNK void takesLaterStruct(const LaterGeneric<swift::Int>& x) noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC16takesLaterStructyyAA0F7GenericVySiGF");
// CHECK-NEXT:   SWIFT_INLINE_THUNK LaterGenericEnum<swift::Int> returnsLaterEnum() noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC16returnsLaterEnumAA0f7GenericG0OySiGyF");
// CHECK-NEXT:   SWIFT_INLINE_THUNK void takesLaterEnum(const LaterGenericEnum<swift::Int>& x) noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC14takesLaterEnumyyAA0f7GenericG0OySiGF");
// CHECK-NEXT:   SWIFT_INLINE_THUNK LaterGeneric<swift::Int> getLaterStruct() noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC11laterStructAA12LaterGenericVySiGvp");
// CHECK-NEXT:   SWIFT_INLINE_THUNK void setLaterStruct(const LaterGeneric<swift::Int>& newValue) noexcept SWIFT_SYMBOL("s:8Generics15ComesFirstClassC11laterStructAA12LaterGenericVySiGvp");
// CHECK-NEXT: protected:

// CHECK: class SWIFT_SYMBOL("s:8Generics16ComesFirstStructV") ComesFirstStruct;
// CHECK: inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstStruct> = true;

// CHECK: class SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO") ComesFirstEnum;
// CHECK: inline const constexpr bool isUsableInGenericContext<Generics::ComesFirstEnum> = true;

// CHECK: class SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO") ComesFirstEnum final {
// CHECK: LaterGeneric<ComesFirstEnum> returnsLaterOpt() const noexcept SWIFT_SYMBOL("s:8Generics14ComesFirstEnumO15returnsLaterOptAA0F7GenericVyACGyF");

// CHECK: namespace Generics SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Generics") {
// CHECK-EMPTY:
// CHECK-NEXT:  namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT:  class _impl_ComesFirstStruct;

// CHECK: class SWIFT_SYMBOL("s:8Generics16ComesFirstStructV") ComesFirstStruct final {
// CHECK: LaterGeneric<ComesFirstStruct> returnsLaterOpt() const noexcept SWIFT_SYMBOL("s:8Generics16ComesFirstStructV15returnsLaterOptAA0F7GenericVyACGyF");

// CHECK: class SWIFT_SYMBOL("s:8Generics22GenericMemberCollisionC") GenericMemberCollision final : public swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT:   static SWIFT_INLINE_THUNK GenericMemberCollision init() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int getItem() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int value() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int shared() noexcept
// CHECK-NEXT:   // skip emitting accessor method for 'item'. 'getItem' already declared.
// CHECK-NEXT: protected:

// CHECK: class SWIFT_SYMBOL("s:8Generics30GenericMemberCollisionReversedC") GenericMemberCollisionReversed final : public swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT:   static SWIFT_INLINE_THUNK GenericMemberCollisionReversed init() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int getItem() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int value() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int shared() noexcept
// CHECK-NEXT:   // skip emitting accessor method for 'item'. 'getItem' already declared.
// CHECK-NEXT: protected:

// CHECK: InheritedUser final : public InheritedMiddle {
// CHECK-NEXT: public:
// CHECK-NEXT:   using InheritedMiddle::InheritedMiddle;
// CHECK-NEXT:   using InheritedMiddle::operator=;
// CHECK-NEXT:   static SWIFT_INLINE_THUNK InheritedUser init() noexcept
// CHECK-NEXT:   // skip emitting accessor method for 'item'. 'getItem' already declared.
// CHECK-NEXT:   SWIFT_INLINE_THUNK LaterGeneric<swift::Int> novel() noexcept
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int read(const LaterGeneric<swift::Int>& value) noexcept
// CHECK-NEXT: protected:

// CHECK: class SWIFT_SYMBOL("s:8Generics12LaterGenericV") LaterGeneric final {

// CHECK: class SWIFT_SYMBOL("s:8Generics16LaterGenericEnumO") LaterGenericEnum final {
// CHECK: ComesFirstClass returnsFirstClass() const noexcept SWIFT_SYMBOL("s:8Generics16LaterGenericEnumO17returnsFirstClassAA05ComesfG0CyF");

// CHECK: class SWIFT_SYMBOL("s:8Generics16LaterGenericUserC") LaterGenericUser : public swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT:   SWIFT_INLINE_THUNK void takesLaterEnum(const LaterGenericEnum<swift::Int>& x) noexcept SWIFT_SYMBOL("s:8Generics16LaterGenericUserC05takesB4EnumyyAA0bcF0OySiGF");
// CHECK-NEXT:   SWIFT_INLINE_THUNK LaterGeneric<LaterGenericUser> returnsLaterStruct() noexcept SWIFT_SYMBOL("s:8Generics16LaterGenericUserC07returnsB6StructAA0bC0VyACGyF");
// CHECK-NEXT: protected:

// CHECK: SWIFT_INLINE_THUNK LaterGenericEnum<swift::Int> ComesFirstClass::returnsLaterEnum() noexcept {
// CHECK: SWIFT_INLINE_THUNK LaterGeneric<LaterGenericUser> LaterGenericUser::returnsLaterStruct() noexcept {

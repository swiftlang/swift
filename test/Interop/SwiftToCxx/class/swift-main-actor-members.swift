// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Members -target %target-swift-5.1-abi-triple -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/members.h
// RUN: %FileCheck %s < %t/members.h

// RUN: %check-interop-cxx-header-in-clang(%t/members.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/members.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR)

// REQUIRES: concurrency

// Main-actor-isolated members are guarded both in the class and in their
// out-of-line definitions. Nonisolated members are not.

// CHECK-LABEL: class SWIFT_SYMBOL("s:7Members14MainActorClassC") MainActorClass final : public swift::_impl::RefCountedClass {
// CHECK:       #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR static SWIFT_INLINE_THUNK MainActorClass init(swift::Int value) noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int getValue() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK void setValue(swift::Int value) noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int getConstant() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR static SWIFT_INLINE_THUNK swift::Int getCounter() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR static SWIFT_INLINE_THUNK void setCounter(swift::Int value) noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int increment() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR static SWIFT_INLINE_THUNK MainActorClass make() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_INLINE_THUNK swift::Int nonisolatedMethod() noexcept
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK void preconcurrencyMethod() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK void renamedMethod() noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR

// CHECK-LABEL: class SWIFT_SYMBOL("s:7Members13MainActorEnumO") MainActorEnum final {
// CHECK:         SWIFT_INLINE_THUNK bool isFirst() const;
// CHECK:       #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int describe() const noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int getComputed() const noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_INLINE_THUNK swift::Int getHashValue() const noexcept

// CHECK-LABEL: class SWIFT_SYMBOL("s:7Members15MainActorStructV") MainActorStruct final {
// CHECK:       SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int operator [](swift::Int i) const noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR

// CHECK-LABEL: class SWIFT_SYMBOL("s:7Members23PartiallyIsolatedStructV") PartiallyIsolatedStruct final {
// CHECK:         SWIFT_INLINE_THUNK void setX(swift::Int value) noexcept
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR static SWIFT_INLINE_THUNK PartiallyIsolatedStruct init(swift::Int mainActorValue) noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK:         SWIFT_INLINE_THUNK swift::Int nonisolatedMethod() const noexcept
// CHECK-NEXT:  #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int mainActorExtensionMethod() const noexcept
// CHECK-NEXT:  #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR

// CHECK:       void GenericBox<T_0_0>::setValue(const T_0_0& value) noexcept {
// CHECK:       #ifdef SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    template<class T_0_0>
// CHECK-NEXT:  #ifdef __cpp_concepts
// CHECK-NEXT:  requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT:  #endif // __cpp_concepts
// CHECK-NEXT:  SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK T_0_0 GenericBox<T_0_0>::isolatedGet() const noexcept {
// CHECK:         swift::_impl::checkMainActorIsolation();
// CHECK:       #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:  template<class T_0_0>
// CHECK:       T_0_0 GenericBox<T_0_0>::nonisolatedGet() const noexcept {
// CHECK-NOT:     checkMainActorIsolation
// CHECK:       }

// CHECK:         SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK MainActorClass MainActorClass::init(swift::Int value) noexcept {
// CHECK-NEXT:    swift::_impl::checkMainActorIsolation();
// CHECK:         SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int MainActorClass::getCounter() noexcept {
// CHECK-NEXT:    swift::_impl::checkMainActorIsolation();
// CHECK:         SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK MainActorClass MainActorClass::make() noexcept {
// CHECK:       #endif // SWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR
// CHECK-NEXT:    SWIFT_INLINE_THUNK swift::Int MainActorClass::nonisolatedMethod() noexcept {
// CHECK-NEXT:    return Members::_impl::$s7Members14MainActorClassC17nonisolatedMethodSiyF(
// CHECK:         SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int MainActorEnum::describe() const noexcept {
// CHECK-NEXT:    swift::_impl::checkMainActorIsolation();
// CHECK:       SWIFT_MAIN_ACTOR SWIFT_INLINE_THUNK swift::Int MainActorStruct::operator [](swift::Int i) const noexcept
// CHECK-NEXT:    swift::_impl::checkMainActorIsolation();

@_expose(Cxx)
@MainActor public final class MainActorClass {
  public init(value: Int) { self.value = value }
  public var value: Int
  public let constant: Int = 7
  public static var counter: Int = 0
  public func increment() -> Int {
    value += 1
    return value
  }
  public static func make() -> MainActorClass { MainActorClass(value: 0) }
  nonisolated public func nonisolatedMethod() -> Int { 42 }
  @preconcurrency @MainActor public func preconcurrencyMethod() {}
  @_expose(Cxx, "renamedMethod") public func originalName() {}
}

public struct PartiallyIsolatedStruct {
  public init() {}
  public var x: Int = 0
  @MainActor public init(mainActorValue: Int) { x = mainActorValue }
  @MainActor public var mainActorProperty: Int {
    get { x }
    set { x = newValue }
  }
  @MainActor public mutating func mainActorMethod(_ v: Int) { x = v }
  public func nonisolatedMethod() -> Int { x }

}

extension PartiallyIsolatedStruct {
  @MainActor public func mainActorExtensionMethod() -> Int { x }
}

@MainActor public struct MainActorStruct {
  public init() {}
  public var y: Int = 1
  public subscript(_ i: Int) -> Int { y + i }
}

@MainActor public enum MainActorEnum {
  case first, second
  public func describe() -> Int { self == .first ? 1 : 2 }
  public var computed: Int { 3 }
}

public struct GenericBox<T> {
  public init(_ value: T) { self.value = value }
  public var value: T
  @MainActor public func isolatedGet() -> T { value }
  public func nonisolatedGet() -> T { value }
}

@MainActor public func mainActorFunc() -> Int { 1 }

@MainActor public func mainActorGenericFunc<T>(_ x: T) -> T { x }

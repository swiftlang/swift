// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -D RESILIENT_MODULE -module-name Resilient -emit-module -emit-module-path %t/Resilient.swiftmodule -enable-library-evolution -clang-header-expose-decls=all-public -emit-clang-header-path %t/resilient.h
// RUN: %target-swift-frontend %s -I %t -module-name Guards -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/guards.h
// RUN: %FileCheck %s < %t/guards.h

// RUN: echo '#include "resilient.h"' > %t/resilient-guards.h
// RUN: cat %t/guards.h >> %t/resilient-guards.h

// The header must compile whether or not the consumer opted into the Swift
// error support. The throwing class method thunk declares an unused `_ctx`.
// RUN: %check-interop-cxx-header-in-clang(%t/resilient-guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/resilient-guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function -Wno-unused-variable)
// RUN: %check-interop-cxx-header-in-clang(%t/resilient-guards.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

#if RESILIENT_MODULE

open class BaseClass {
  public init() {}
  open func baseMethod() {}
}

#else

import Resilient

public func ordinary() -> Int { 42 }

public func throwing() throws -> Int { 42 }

public struct Guarded {
  public var value: Int

  public init(_ value: Int) { self.value = value }

  public init(checked value: Int, flag: Bool) throws { self.value = value }

  public func checked() throws -> Int { value }

  public static func checkedStatic() throws -> Int { 42 }

  public func unchecked() -> Int { value }
}

// The first new method of a subclass of a resilient class emits the class
// metadata base offset, which the non-throwing method below uses as well.
public class Derived: BaseClass {
  override public init() {}
  public func throwingInDerived() throws {}
  public func ordinaryInDerived() {}
}

#endif

// The C declarations don't use the Swift error support types and stay
// unguarded, including the class metadata base offset that is emitted once
// for the first method of Derived.
// CHECK-LABEL: namespace Guards SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Guards") {
// CHECK-NEXT:  namespace _impl {
// CHECK-NOT: SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// CHECK:      SWIFT_EXTERN void $s6Guards7DerivedC010throwingInB0yyKF({{.*}}) SWIFT_CALL; // throwingInDerived()
// CHECK-NEXT: SWIFT_EXTERN uint{{[0-9]+}}_t $s6Guards7DerivedCMo; // class metadata base offset
// CHECK-NEXT: SWIFT_EXTERN void $s6Guards7DerivedC010ordinaryInB0yyF({{.*}}) SWIFT_NOEXCEPT SWIFT_CALL; // ordinaryInDerived()
// CHECK-NOT: SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// CHECK:      SWIFT_EXTERN ptrdiff_t $s6Guards8throwingSiyKF({{.*}}) SWIFT_CALL; // throwing()
// CHECK-NOT: SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR

// The C++ member declarations and definitions of throwing functions are
// guarded.
// CHECK-LABEL: class SWIFT_SYMBOL("s:6Guards7DerivedC") Derived : public Resilient::BaseClass {
// CHECK:      #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<void> throwingInDerived() SWIFT_SYMBOL("s:6Guards7DerivedC010throwingInB0yyKF");
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK void ordinaryInDerived() noexcept SWIFT_SYMBOL("s:6Guards7DerivedC010ordinaryInB0yyF");

// CHECK-LABEL: class SWIFT_SYMBOL("s:6Guards7GuardedV") Guarded final {
// CHECK:        static SWIFT_INLINE_THUNK Guarded init(swift::Int value) noexcept SWIFT_SYMBOL("s:6Guards7GuardedVyACSicfc");
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   static SWIFT_INLINE_THUNK swift::ThrowingResult<Guarded> init(swift::Int value, bool flag) SWIFT_SYMBOL("s:6Guards7GuardedV7checked4flagACSi_SbtKcfc");
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> checked() const SWIFT_SYMBOL("s:6Guards7GuardedV7checkedSiyKF");
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   static SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> checkedStatic() SWIFT_SYMBOL("s:6Guards7GuardedV13checkedStaticSiyKFZ");
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int unchecked() const noexcept SWIFT_SYMBOL("s:6Guards7GuardedV9uncheckedSiyF");

// CHECK:      SWIFT_INLINE_THUNK swift::Int ordinary() noexcept SWIFT_SYMBOL("s:6Guards8ordinarySiyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Guards::_impl::$s6Guards8ordinarySiyF();
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> throwing() SWIFT_SYMBOL("s:6Guards8throwingSiyKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK:      #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-EMPTY:
// CHECK-NEXT:   SWIFT_INLINE_THUNK Derived Derived::init() noexcept {

// CHECK:      #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<void> Derived::throwingInDerived() {
// CHECK:      #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK void Derived::ordinaryInDerived() noexcept {
// CHECK:      FTypeAddress *fptrptr_ = reinterpret_cast<FTypeAddress *>(vtable_ + (_impl::$s6Guards7DerivedCMo + {{[0-9]+}}) / sizeof(void *));

// CHECK:        SWIFT_INLINE_THUNK Guarded Guarded::init(swift::Int value) noexcept {
// CHECK:      #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<Guarded> Guarded::init(swift::Int value, bool flag) {
// CHECK:      #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> Guarded::checked() const {
// CHECK:      #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> Guarded::checkedStatic() {
// CHECK:      #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int Guarded::unchecked() const noexcept {

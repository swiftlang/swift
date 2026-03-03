// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public class C {
    public var x: Int
    public init(x: Int) { self.x = x }
}

public enum E {
    case c(C)
    case i(Int)
}

extension E {
  public func matchesIntValue(_ value: Int) -> Bool {
    switch self {
    case .c:
      return false

    case .i(let mine):
      return mine == value
    }
  }
}

public enum F {
  case a(Int)
  case b([C])
}

public enum G<T> {
  case a(Int)
  case b([T])
}

// CHECK:      SWIFT_INLINE_THUNK E E::_impl_c::operator()(const C& val) const {
// CHECK-NEXT:   auto result = E::_make();
// CHECK-NEXT:   auto op = swift::_impl::_impl_RefCountedClass::copyOpaquePointer(val);
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &op, sizeof(op));
// CHECK-NEXT:   result._destructiveInjectEnumTag(0);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK C E::getC() const {
// CHECK-NEXT:   if (!isC()) abort();
// CHECK-NEXT:   alignas(E) unsigned char buffer[sizeof(E)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) E(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   return swift::_impl::implClassFor<C>::type::makeRetained(*reinterpret_cast<void **>(payloadFromDestruction));
// CHECK-NEXT: }

// CHECK: SWIFT_INLINE_THUNK bool E::matchesIntValue(swift::Int value) const {
// CHECK-NEXT: return Enums::_impl::$s5Enums1EO15matchesIntValueySbSiF(value, Enums::_impl::swift_interop_passDirect_Enums_{{.*}}(_getOpaquePointer()));

// CHECK: SWIFT_INLINE_THUNK swift::Array<C> F::getB() const {
// CHECK-NEXT:    if (!isB()) abort();
// CHECK-NEXT:    alignas(F) unsigned char buffer[sizeof(F)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) F(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    return swift::_impl::implClassFor<swift::Array<C>>::type::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT:      swift::_impl::implClassFor<swift::Array<C>>::type::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:    });
// CHECK-NEXT:  }

// CHECK:   SWIFT_INLINE_THUNK swift::Array<T_0_0> G<T_0_0>::getB() const {
// CHECK-NEXT:     if (!isB()) abort();
// CHECK-NEXT:     alignas(G) unsigned char buffer[sizeof(G)];
// CHECK-NEXT:     auto *thisCopy = new(buffer) G(*this);
// CHECK-NEXT:     char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:     return swift::_impl::implClassFor<swift::Array<T_0_0>>::type::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT:       swift::_impl::implClassFor<swift::Array<T_0_0>>::type::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:     });
// CHECK-NEXT:   }

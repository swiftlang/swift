// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-decls=all-public -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

// rdar://124466216
// UNSUPPORTED: OS=watchos

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
// CHECK-NEXT: return _impl::$s5Enums1EO15matchesIntValueySbSiF(value, _impl::swift_interop_passDirect_Enums_uint64_t_0_8_uint8_t_8_9(_getOpaquePointer()));

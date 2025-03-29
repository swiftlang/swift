// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Operators -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/operators.h
// RUN: %FileCheck %s < %t/operators.h

// RUN: %check-interop-cxx-header-in-clang(%t/operators.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/operators.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++23)

// CHECK-LABEL: namespace Operators SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Operators") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN bool $s9Operators2eeoiySbAA6IntBoxV_ADtF(struct swift_interop_passStub_Operators_uint32_t_0_4 lhs, struct swift_interop_passStub_Operators_uint32_t_0_4 rhs) SWIFT_NOEXCEPT SWIFT_CALL; // ==(_:_:)

// CHECK: }

public struct IntBox {
  var x: CInt

  public subscript(x: CInt) -> CInt {
    return x
  } 

  public subscript(x: CInt, _: CInt) -> CInt {
    return x
  } 
}

public struct CustomArray<Element> where Element : ~Copyable {
  private var buffer: UnsafeMutableBufferPointer<Element>

  public subscript(index: Int) -> Element {
    _read {
        yield buffer[index]
    }
    nonmutating _modify {
        yield &buffer[index]
    }
  }
}

// CHECK: #if __cplusplus >= 202302L
// CHECK-NEXT: SWIFT_INLINE_THUNK int operator [](int x, int _2) const SWIFT_SYMBOL("s:9Operators6IntBoxVys5Int32VAE_AEtcig");
// CHECK-NEXT: #endif // #if __cplusplus >= 202302L

public func -(lhs: IntBox, rhs: IntBox) -> CInt {
  return lhs.x - rhs.x
}

// CHECK: SWIFT_INLINE_THUNK int operator-(const IntBox& lhs, const IntBox& rhs) noexcept SWIFT_SYMBOL("s:9Operators1soiys5Int32VAA6IntBoxV_AFtF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Operators::_impl::$s9Operators1soiys5Int32VAA6IntBoxV_AFtF(Operators::_impl::swift_interop_passDirect_Operators_uint32_t_0_4(Operators::_impl::_impl_IntBox::getOpaquePointer(lhs)), Operators::_impl::swift_interop_passDirect_Operators_uint32_t_0_4(Operators::_impl::_impl_IntBox::getOpaquePointer(rhs)));
// CHECK-NEXT: }

public func ==(lhs: IntBox, rhs: IntBox) -> Bool {
  return lhs.x == rhs.x
}

// CHECK: SWIFT_INLINE_THUNK bool operator==(const IntBox& lhs, const IntBox& rhs) noexcept SWIFT_SYMBOL("s:9Operators2eeoiySbAA6IntBoxV_ADtF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Operators::_impl::$s9Operators2eeoiySbAA6IntBoxV_ADtF(Operators::_impl::swift_interop_passDirect_Operators_uint32_t_0_4(Operators::_impl::_impl_IntBox::getOpaquePointer(lhs)), Operators::_impl::swift_interop_passDirect_Operators_uint32_t_0_4(Operators::_impl::_impl_IntBox::getOpaquePointer(rhs)));
// CHECK-NEXT: }


// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Operators -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/operators.h
// RUN: %FileCheck %s < %t/operators.h
// RUN: %FileCheck %s --check-prefix=DEFS \
// RUN:   --implicit-check-not='bool CollidingSubscripts::operator' \
// RUN:   --implicit-check-not='double CollidingSubscripts::operator' \
// RUN:   --implicit-check-not='float CollidingSubscripts::operator' < %t/operators.h
// RUN: %if OS_FAMILY=darwin %{ %FileCheck %s --check-prefix=INT-DISTINCT < %t/operators.h %}
// RUN: %if OS_FAMILY=linux || OS_FAMILY=windows %{ %FileCheck %s --check-prefix=INT-SAME \
// RUN:   --implicit-check-not='swift::Int CollidingSubscripts::operator' < %t/operators.h %}

// RUN: %check-interop-cxx-header-in-clang(%t/operators.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/operators.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++23)

// CHECK-LABEL: namespace Operators SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Operators") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN bool $s9Operators2eeoiySbAA6IntBoxV_ADtF(struct swift_interop_passStub_Operators_uint32_t_0_4 lhs, struct swift_interop_passStub_Operators_uint32_t_0_4 rhs) SWIFT_NOEXCEPT SWIFT_CALL; // ==(_:_:)

// CHECK: }

public struct CollidingSubscripts {
  var x: CInt

  public subscript(x: CInt) -> CInt {
    return x
  }

  // Differs from the subscript above only in its result type.
  public subscript(x: CInt) -> Bool {
    return true
  }

  // CInt and Int32 are the same C++ type.
  public subscript(x: Int32) -> Double {
    return 0
  }

  // Argument labels are not part of the C++ signature.
  public subscript(index x: CInt) -> CInt {
    return x
  }

  // Int8 is `signed char`, which is a different C++ type than `char` on every
  // target.
  public subscript(x: Int8) -> CInt {
    return 0
  }

  public subscript(x: CChar) -> CChar {
    return 0
  }

  // CSignedChar and Int8 are the same C++ type.
  public subscript(x: CSignedChar) -> Bool {
    return true
  }

  // Int64 is a different C++ type than int on every target.
  public subscript(x: Int64) -> CInt {
    return 0
  }

  // Int is `long` on Darwin, which is a different C++ type than Int32 (`int`)
  // and Int64 (`long long`). On Linux and Windows, Int is the same C++ type as
  // one of them.
  public subscript(x: Int) -> Int {
    return x
  }

  public typealias Alias = CollidingSubscripts

  public subscript(x: CollidingSubscripts) -> CInt {
    return 0
  }

  // A type alias is the same C++ type as the type it aliases.
  public subscript(x: Alias) -> Bool {
    return true
  }
}

// CHECK: class SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsV") CollidingSubscripts final {
// CHECK: SWIFT_INLINE_THUNK int operator [](int x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32VAEcig");
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Double'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: // skip emitting subscript 'subscript(index:) -> CInt'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: SWIFT_INLINE_THUNK int operator [](int8_t x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32Vs4Int8Vcig");
// CHECK-NEXT: SWIFT_INLINE_THUNK char operator [](char x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys4Int8VAEcig");
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: SWIFT_INLINE_THUNK int operator [](int64_t x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32Vs5Int64Vcig");
// CHECK: SWIFT_INLINE_THUNK int operator [](const CollidingSubscripts& x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32VACcig");
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Float'. 'operator []' with the same parameter types already declared.

// DEFS: SWIFT_INLINE_THUNK int CollidingSubscripts::operator [](int x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32VAEcig") {
// DEFS: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// DEFS-NEXT: // skip emitting subscript 'subscript(_:) -> Double'. 'operator []' with the same parameter types already declared.
// DEFS-NEXT: // skip emitting subscript 'subscript(index:) -> CInt'. 'operator []' with the same parameter types already declared.
// DEFS-NEXT: SWIFT_INLINE_THUNK int CollidingSubscripts::operator [](int8_t x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32Vs4Int8Vcig") {
// DEFS: SWIFT_INLINE_THUNK char CollidingSubscripts::operator [](char x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys4Int8VAEcig") {
// DEFS: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// DEFS-NEXT: SWIFT_INLINE_THUNK int CollidingSubscripts::operator [](int64_t x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32Vs5Int64Vcig") {
// DEFS: SWIFT_INLINE_THUNK int CollidingSubscripts::operator [](const CollidingSubscripts& x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVys5Int32VACcig") {
// DEFS: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// DEFS-NEXT: // skip emitting subscript 'subscript(_:) -> Float'. 'operator []' with the same parameter types already declared.

// INT-DISTINCT: SWIFT_INLINE_THUNK int operator [](int64_t x) const noexcept
// INT-DISTINCT-NEXT: SWIFT_INLINE_THUNK swift::Int operator [](swift::Int x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVyS2icig");
// INT-DISTINCT: SWIFT_INLINE_THUNK swift::Int CollidingSubscripts::operator [](swift::Int x) const noexcept SWIFT_SYMBOL("s:9Operators19CollidingSubscriptsVyS2icig") {

// INT-SAME: SWIFT_INLINE_THUNK int operator [](int64_t x) const noexcept
// INT-SAME-NEXT: // skip emitting subscript 'subscript(_:) -> Int'. 'operator []' with the same parameter types already declared.
// INT-SAME: SWIFT_INLINE_THUNK int CollidingSubscripts::operator [](int64_t x) const noexcept
// INT-SAME: // skip emitting subscript 'subscript(_:) -> Int'. 'operator []' with the same parameter types already declared.

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
// CHECK-NEXT: SWIFT_INLINE_THUNK int operator [](int x, int _2) const noexcept SWIFT_SYMBOL("s:9Operators6IntBoxVys5Int32VAE_AEtcig");
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

public func ===(lhs: IntBox, rhs: IntBox) -> Bool {
  return lhs.x == rhs.x
}
// CHECK-NOT: operator===

infix operator +>
func +> (lhs: IntBox, rhs: Int) -> IntBox {
  return IntBox(x: lhs.x + CInt(rhs))
}
// CHECK-NOT: operator+>

infix operator !
func ! (lhs: IntBox, rhs: IntBox) -> Bool {
  return lhs.x == rhs.x
}
// CHECK-NOT: operator!

// This extension is at the end of the file so that it does not change the
// order in which the declarations above are printed.
extension CollidingSubscripts {
  // Subscripts from extensions are checked against the subscripts of the type.
  public subscript(x: CInt) -> Float {
    return 0
  }
}

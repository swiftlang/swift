// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/useopt.h

// RUN: %FileCheck %s < %t/useopt.h

@_expose(Cxx)
public struct SmallStruct {
    let x: Int16
}

@_expose(Cxx)
public class Klass {
    let x: Int16

    init(_ x: Int16) {
        self.x = x
    }
}

@_expose(Cxx)
public func createCIntOpt(_ val: CInt) -> Optional<CInt> {
    return val
}

@_expose(Cxx)
public func takeCIntOpt(_ val: Optional<CInt>) {
    print(String(describing: val))
}

@_expose(Cxx)
public func createSmallStructOpt(_ val: Int16) -> SmallStruct? {
    return SmallStruct(x: val)
}

@_expose(Cxx)
public func takeSmallStructOpt(_ val: Optional<SmallStruct>) {
    print(String(describing: val))
}

@_expose(Cxx)
public func createKlassOpt(_ val: Int16) -> Klass? {
    return Klass(val)
}

@_expose(Cxx)
public func takeKlassOpt(_ val: Klass?) {
    print(String(describing: val))
}

@_expose(Cxx)
public func resetOpt<T>(_ val: inout Optional<T>) {
    val = .none
}


// CHECK: inline Swift::Optional<int> createCIntOpt(int val) noexcept SWIFT_WARN_UNUSED_RESULT {
 // CHECK-NEXT: return Swift::_impl::_impl_Optional<int>::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:   _impl::swift_interop_returnDirect_UseOptional_[[CINTENC:[a-z0-9_]+]](result, _impl::$s11UseOptional13createCIntOptys5Int32VSgADF(val));
// CHECK-NEXT: });
// CHECK-NEXT: }


// CHECK: inline Swift::Optional<Klass> createKlassOpt(int16_t val) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Swift::_impl::_impl_Optional<Klass>::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT: _impl::swift_interop_returnDirect_UseOptional_[[CLASSENC:[a-z0-9_]+]](result, _impl::$s11UseOptional14createKlassOptyAA0D0CSgs5Int16VF(val));
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: inline Swift::Optional<SmallStruct> createSmallStructOpt(int16_t val) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return Swift::_impl::_impl_Optional<SmallStruct>::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::swift_interop_returnDirect_UseOptional_uint32_t_0_4(result, _impl::$s11UseOptional20createSmallStructOptyAA0dE0VSgs5Int16VF(val));
// CHECK-NEXT:   });
// CHECK-NEXT: }


// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif
// CHECK-NEXT: inline void resetOpt(Swift::Optional<T_0_0>& val) noexcept {
// CHECK-NEXT: #ifndef __cpp_concepts
// CHECK-NEXT: static_assert(swift::isUsableInGenericContext<T_0_0>, "type cannot be used in a Swift generic context");
// CHECK-NEXT: #endif
// CHECK-NEXT:   return _impl::$s11UseOptional8resetOptyyxSgzlF(Swift::_impl::_impl_Optional<T_0_0>::getOpaquePointer(val), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata());
// CHECK-NEXT: }


// CHECK: inline void takeCIntOpt(const Swift::Optional<int>& val) noexcept {
// CHECK-NEXT:  return _impl::$s11UseOptional11takeCIntOptyys5Int32VSgF(_impl::swift_interop_passDirect_UseOptional_[[CINTENC]](Swift::_impl::_impl_Optional<int>::getOpaquePointer(val)));
// CHECK-NEXT: }


// CHECK: inline void takeKlassOpt(const Swift::Optional<Klass>& val) noexcept {
// CHECK-NEXT:   return _impl::$s11UseOptional12takeKlassOptyyAA0D0CSgF(_impl::swift_interop_passDirect_UseOptional_[[CLASSENC]](Swift::_impl::_impl_Optional<Klass>::getOpaquePointer(val)));
// CHECK-NEXT: }


// CHECK: inline void takeSmallStructOpt(const Swift::Optional<SmallStruct>& val) noexcept {
// CHECK-NEXT:  return _impl::$s11UseOptional18takeSmallStructOptyyAA0dE0VSgF(_impl::swift_interop_passDirect_UseOptional_uint32_t_0_4(Swift::_impl::_impl_Optional<SmallStruct>::getOpaquePointer(val)));
// CHECK-NEXT: }

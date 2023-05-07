// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

@_expose(Cxx)
public class C {
    public var x: UInt64
    public init(x: UInt64) { self.x = x }
}

@_expose(Cxx)
public struct S {
    public var x: UInt64
    public init(x: UInt64) { self.x = x }
}

@_expose(Cxx)
public indirect enum IndirectEnum {
    case empty
    case ui64(UInt64)
    case d(Double)
    case s(S)
    case c(C)
    case ie(IndirectEnum)
}

// CHECK:      SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::_impl_ui64::operator()(uint64_t val) const {
// CHECK-NEXT:   auto result = IndirectEnum::_make();
// CHECK-NEXT:   auto metadata = swift::TypeMetadataTrait<uint64_t>::getTypeMetadata();
// CHECK-NEXT:   auto allocBoxResult = swift::_impl::swift_allocBox(metadata);
// CHECK-NEXT:   auto src = val;
// CHECK-NEXT:   memcpy(allocBoxResult.opaquePtr, &src, sizeof(src));
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &allocBoxResult.refCountedPtr, sizeof(allocBoxResult.refCountedPtr));
// CHECK-NEXT:   result._destructiveInjectEnumTag(0);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::_impl_d::operator()(double val) const {
// CHECK-NEXT:   auto result = IndirectEnum::_make();
// CHECK-NEXT:   auto metadata = swift::TypeMetadataTrait<double>::getTypeMetadata();
// CHECK-NEXT:   auto allocBoxResult = swift::_impl::swift_allocBox(metadata);
// CHECK-NEXT:   auto src = val;
// CHECK-NEXT:   memcpy(allocBoxResult.opaquePtr, &src, sizeof(src));
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &allocBoxResult.refCountedPtr, sizeof(allocBoxResult.refCountedPtr));
// CHECK-NEXT:   result._destructiveInjectEnumTag(1);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::_impl_s::operator()(const S& val) const {
// CHECK-NEXT:   auto result = IndirectEnum::_make();
// CHECK-NEXT:   auto metadata = swift::TypeMetadataTrait<S>::getTypeMetadata();
// CHECK-NEXT:   auto allocBoxResult = swift::_impl::swift_allocBox(metadata);
// CHECK-NEXT:   alignas(S) unsigned char buffer[sizeof(S)];
// CHECK-NEXT:   auto *valCopy = new(buffer) S(val);
// CHECK-NEXT:   swift::_impl::implClassFor<S>::type::initializeWithTake(static_cast<char * _Nonnull>(allocBoxResult.opaquePtr), swift::_impl::implClassFor<S>::type::getOpaquePointer(*valCopy));
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &allocBoxResult.refCountedPtr, sizeof(allocBoxResult.refCountedPtr));
// CHECK-NEXT:   result._destructiveInjectEnumTag(2);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::_impl_c::operator()(const C& val) const {
// CHECK-NEXT:   auto result = IndirectEnum::_make();
// CHECK-NEXT:   auto metadata = swift::TypeMetadataTrait<C>::getTypeMetadata();
// CHECK-NEXT:   auto allocBoxResult = swift::_impl::swift_allocBox(metadata);
// CHECK-NEXT:   auto src = swift::_impl::_impl_RefCountedClass::copyOpaquePointer(val);
// CHECK-NEXT:   memcpy(allocBoxResult.opaquePtr, &src, sizeof(src));
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &allocBoxResult.refCountedPtr, sizeof(allocBoxResult.refCountedPtr));
// CHECK-NEXT:   result._destructiveInjectEnumTag(3);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::_impl_ie::operator()(const IndirectEnum& val) const {
// CHECK-NEXT:   auto result = IndirectEnum::_make();
// CHECK-NEXT:   auto metadata = swift::TypeMetadataTrait<IndirectEnum>::getTypeMetadata();
// CHECK-NEXT:   auto allocBoxResult = swift::_impl::swift_allocBox(metadata);
// CHECK-NEXT:   alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:   auto *valCopy = new(buffer) IndirectEnum(val);
// CHECK-NEXT:   swift::_impl::implClassFor<IndirectEnum>::type::initializeWithTake(static_cast<char * _Nonnull>(allocBoxResult.opaquePtr), swift::_impl::implClassFor<IndirectEnum>::type::getOpaquePointer(*valCopy));
// CHECK-NEXT:   memcpy(result._getOpaquePointer(), &allocBoxResult.refCountedPtr, sizeof(allocBoxResult.refCountedPtr));
// CHECK-NEXT:   result._destructiveInjectEnumTag(4);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

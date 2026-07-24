// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: PTRSIZE=64

public enum MatchResult {
    case none
    case scalar(Unicode.Scalar?)
    case value(Int32?)
    case rawptr(UnsafeRawPointer?)
}

// An optional value-type payload is returned via returnNewValue, not by
// default-constructing swift::Optional<...>.

// CHECK:      SWIFT_INLINE_THUNK swift::Optional<char32_t> MatchResult::getScalar() const {
// CHECK:        return swift::_impl::implClassFor<swift::Optional<char32_t>>::type::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT:     swift::_impl::implClassFor<swift::Optional<char32_t>>::type::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:   });

// CHECK:      SWIFT_INLINE_THUNK swift::Optional<int32_t> MatchResult::getValue() const {
// CHECK:        return swift::_impl::implClassFor<swift::Optional<int32_t>>::type::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT:     swift::_impl::implClassFor<swift::Optional<int32_t>>::type::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:   });

// An optional pointer payload bridges to a nullable C++ pointer, which is
// trivially default-constructible, so it keeps the memcpy path.

// CHECK:      SWIFT_INLINE_THUNK void const * _Nullable MatchResult::getRawptr() const {
// CHECK:        void const * _Nullable result;
// CHECK-NEXT:   memcpy(&result, payloadFromDestruction, sizeof(result));
// CHECK-NEXT:   return result;

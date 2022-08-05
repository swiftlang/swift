// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/enums.swift -module-name Enums -emit-module -emit-module-path %t/Enums.swiftmodule -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h

// RUN: %target-swift-frontend %s -typecheck -module-name UsesEnums -I %t -clang-header-expose-public-decls -emit-clang-header-path %t/uses-enums.h

// FIXME: add import automatically?
// RUN: echo '#include "enums.h"' > %t/fixed-uses-enums.h
// RUN: cat %t/uses-enums.h     >> %t/fixed-uses-enums.h

// RUN: %FileCheck %s < %t/uses-enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/fixed-uses-enums.h)

import Enums

public struct UsesEnumsLargeEnum {
    public func passThroughStructSeveralI64(_ y: LargeEnum) -> LargeEnum {
        return y
    }

    public let x: LargeEnum
}

// CHECK: inline Enums::LargeEnum UsesEnumsLargeEnum::passThroughStructSeveralI64(const Enums::LargeEnum& y) const {
// CHECK-NEXT: return Enums::_impl::_impl_LargeEnum::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s9UsesEnums0aB9LargeEnumV27passThroughStructSeveralI64y0B00cD0OAGF(result, Enums::_impl::_impl_LargeEnum::getOpaquePointer(y), _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: inline Enums::LargeEnum UsesEnumsLargeEnum::getX() const {
// CHECK-NEXT: return Enums::_impl::_impl_LargeEnum::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s9UsesEnums0aB9LargeEnumV1x0B00cD0Ovg(result, _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }


public func inoutLargeEnum(_ s: inout LargeEnum) {
    return s = LargeEnum.B
}

// CHECK: inline void inoutLargeEnum(Enums::LargeEnum& s) noexcept {
// CHECK-NEXT: return _impl::$s9UsesEnums14inoutLargeEnumyy0B00dE0OzF(Enums::_impl::_impl_LargeEnum::getOpaquePointer(s));
// CHECK-NEXT: }

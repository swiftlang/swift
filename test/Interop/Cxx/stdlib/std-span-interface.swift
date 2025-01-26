// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature LifetimeDependence -enable-experimental-feature Span -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++20 -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s < %t/interface.swift

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Span
// REQUIRES: swift_feature_LifetimeDependence

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

#if !BRIDGING_HEADER
import StdSpan
#endif
import CxxStdlib

// CHECK:      func funcWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: func funcWithSafeWrapper2(_ s: borrowing ConstSpanOfInt) -> ConstSpanOfInt
// CHECK-NEXT: func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> ConstSpanOfInt
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT: @lifetime(s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper2(_ s: borrowing Span<CInt>) -> Span<CInt>
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> Span<CInt>

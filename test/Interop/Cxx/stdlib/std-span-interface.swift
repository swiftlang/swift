// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature LifetimeDependence -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++20 -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s < %t/interface.swift

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

#if !BRIDGING_HEADER
import StdSpan
#endif
import CxxStdlib

// CHECK:     struct DependsOnSelf {
// CHECK:       @lifetime(borrow self)
// CHECK-NEXT:  @_alwaysEmitIntoClient @_disfavoredOverload public borrowing func get() -> Span<CInt>
// CHECK-NEXT:  borrowing func get() -> ConstSpanOfInt

// CHECK:      mutating func set(_ x: borrowing std.{{.*}}vector<CInt, std.{{.*}}allocator<CInt>>)
// CHECK:      func funcWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: func funcWithSafeWrapper2(_ s: ConstSpanOfInt) -> ConstSpanOfInt
// CHECK-NEXT: func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> ConstSpanOfInt
// CHECK:      struct X {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @_alwaysEmitIntoClient public mutating func methodWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func methodWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: }
// CHECK: @_alwaysEmitIntoClient public func funcWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT: @lifetime(s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper2(_ s: Span<CInt>) -> Span<CInt>
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> Span<CInt>
// CHECK-NEXT: @lifetime(p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper1(_ p: Span<Int32>) -> Span<CInt>
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt, _ len: Int32) -> Span<Int32>
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper3(_ s: Span<CInt>, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper4(_ s: Span<CInt>, _ p: Span<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ p: Span<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper7(_ p: UnsafeBufferPointer<Int32>) -> ConstSpanOfInt

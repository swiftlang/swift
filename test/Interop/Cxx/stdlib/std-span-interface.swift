// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++20 -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s < %t/interface.swift

// REQUIRES: swift_feature_SafeInteropWrappers

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
// CHECK-NEXT:   @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @_alwaysEmitIntoClient public mutating func methodWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func methodWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: }
// CHECK: struct SpanWithoutTypeAlias {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @lifetime(borrow self)
// CHECK-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func bar() -> Span<CInt>
// CHECK-NEXT:   mutating func bar() -> std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>
// CHECK-NEXT:   @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @_alwaysEmitIntoClient public mutating func foo(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func foo(_ s: std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>)
// CHECK-NEXT: }

// CHECK:      @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func FuncWithMutableSafeWrapper(_ s: inout MutableSpan<CInt>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy s)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func FuncWithMutableSafeWrapper2(_ s: inout MutableSpan<CInt>) -> MutableSpan<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper3(_ v: inout VecOfInt) -> MutableSpan<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper1(_ p: inout MutableSpan<Int32>) -> MutableSpan<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper2(_ v: inout VecOfInt, _ len: Int32) -> MutableSpan<Int32>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper3(_ s: inout MutableSpan<CInt>, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper4(_ s: inout MutableSpan<CInt>, _ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper5(_ s: SpanOfInt, _ p: inout MutableSpan<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper6(_ s: SpanOfInt, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func MixedFuncWithMutableSafeWrapper7(_ p: UnsafeMutableBufferPointer<Int32>) -> SpanOfInt

// CHECK:      @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient public func funcWithSafeWrapper2(_ s: Span<CInt>) -> Span<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> Span<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper1(_ p: Span<Int32>) -> Span<CInt>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt, _ len: Int32) -> Span<Int32>
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper3(_ s: Span<CInt>, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper4(_ s: Span<CInt>, _ p: Span<Int32>)
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ p: Span<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutableBufferPointer<Int32>)
// CHECK-NEXT: @_alwaysEmitIntoClient public func mixedFuncWithSafeWrapper7(_ p: UnsafeBufferPointer<Int32>) -> ConstSpanOfInt

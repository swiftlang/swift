// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++20 -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s < %t/interface.swift

// Make sure we trigger typechecking and SIL diagnostics
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -strict-memory-safety -warnings-as-errors -verify -Xcc -std=c++20 %s

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu, OS=linux-android, OS=linux-androideabi

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
// CHECK-NEXT:   /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:   @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func methodWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func methodWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: }
// CHECK: struct SpanWithoutTypeAlias {
// CHECK-NEXT:   init()
// CHECK-NEXT:   mutating func bar() -> std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>
// CHECK-NEXT:   mutating func foo(_ s: std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>)
// CHECK-NEXT:   mutating func otherTemplatedType(_ copy: ConstSpanOfInt, _: S<CInt>)
// CHECK-NEXT:   mutating func otherTemplatedType2(_ copy: ConstSpanOfInt, _: UnsafeMutablePointer<S<CInt>>!)
// CHECK-NEXT: }

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper(_ s: inout MutableSpan<CInt>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy s)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper2(_ s: inout MutableSpan<CInt>) -> MutableSpan<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(&v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper3(_ v: inout VecOfInt) -> MutableSpan<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper1(_ p: inout MutableSpan<Int32>) -> MutableSpan<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(&v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper2(_ v: inout VecOfInt, _ len: Int32) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper3(_ s: inout MutableSpan<CInt>, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(s: copy s)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper4(_ s: inout MutableSpan<CInt>, _ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper5(_ s: SpanOfInt, _ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper6(_ s: SpanOfInt, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper7(_ p: UnsafeMutableBufferPointer<Int32>) -> SpanOfInt

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func `func`(_ copy: Span<CInt>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper(_ s: Span<CInt>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper2(_ s: Span<CInt>) -> Span<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> Span<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper1(_ p: Span<Int32>) -> Span<CInt>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(borrow v)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt, _ len: Int32) -> Span<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper3(_ s: Span<CInt>, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper4(_ s: Span<CInt>, _ p: Span<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ p: Span<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper7(_ p: UnsafeBufferPointer<Int32>) -> ConstSpanOfInt

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @lifetime(copy: copy copy)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mutableKeyword(_ copy: inout MutableSpan<CInt>)

func callMethodWithSafeWrapper(_ x: inout X, s: Span<CInt>) {
    x.methodWithSafeWrapper(s)
}

func callFooBar(_ x: inout SpanWithoutTypeAlias, _ s: ConstSpanOfInt) {
    let _: Span<CInt> = x.bar() // expected-error {{cannot convert value of type}}
    unsafe x.foo(s)
}

@lifetime(span: copy span)
func callFuncWithMutableSafeWrapper(_ span: inout MutableSpan<CInt>, ) {
    FuncWithMutableSafeWrapper(&span)
}

@lifetime(span: copy span)
func callFuncWithMutableSafeWrapper2(_ span: inout MutableSpan<CInt>, ) {
    let _: MutableSpan<CInt> = FuncWithMutableSafeWrapper2(&span)
}

@lifetime(span: copy span)
func callMixedFuncWithMutableSafeWrapper1(_ span: inout MutableSpan<CInt>, ) {
    let _: MutableSpan<CInt> = MixedFuncWithMutableSafeWrapper1(&span)
}

func MixedFuncWithMutableSafeWrapper2(_ v: VecOfInt) {
    var v2 = v
    let _ = MixedFuncWithMutableSafeWrapper2(&v2, 37)
}

@lifetime(span: copy span)
func callMixedFuncWithMutableSafeWrapper3(_ span: inout MutableSpan<CInt>, _ p: UnsafeMutableBufferPointer<CInt>) {
    unsafe MixedFuncWithMutableSafeWrapper3(&span, p)
}

@lifetime(span1: copy span2)
@lifetime(span2: copy span2)
func callMixedFuncWithMutableSafeWrapper4(_ span1: inout MutableSpan<CInt>, _ span2: inout MutableSpan<CInt>) {
    MixedFuncWithMutableSafeWrapper4(&span1, &span2)
}

@lifetime(span: copy span)
func callMixedFuncWithMutableSafeWrapper5(_ span: inout MutableSpan<CInt>, _ s: SpanOfInt) {
    unsafe MixedFuncWithMutableSafeWrapper5(s, &span)
}

func callMixedFuncWithMutableSafeWrapper6(_ s: SpanOfInt, _ p: UnsafeMutableBufferPointer<CInt>) {
    unsafe MixedFuncWithMutableSafeWrapper6(s, p)
}

func callMixedFuncWithMutableSafeWrapper7(_ p: UnsafeMutableBufferPointer<CInt>) {
    let _ = unsafe MixedFuncWithMutableSafeWrapper7(p)
}

func callFuncWithSafeWrapper(_ s: Span<CInt>) {
    funcWithSafeWrapper(s)
}

func callFuncWithSafeWrapper2(_ s: Span<CInt>) {
    let _ = funcWithSafeWrapper2(s)
}

func callFuncWithSafeWrapper3(_ v: borrowing VecOfInt) {
    let _: Span<CInt> = funcWithSafeWrapper3(v)
}

func callMixedFuncWithSafeWrapper1(_ s: Span<CInt>) {
    let _: Span<CInt> = mixedFuncWithSafeWrapper1(s)
}

func callMixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt) {
    let _: Span<CInt> = mixedFuncWithSafeWrapper2(v, 73)
}

func callMixedFuncWithSafeWrapper3(_ s: Span<CInt>, _ p: UnsafeMutableBufferPointer<CInt>) {
    unsafe mixedFuncWithSafeWrapper3(s, p)
}

func callMixedFuncWithSafeWrapper4(_ s: Span<CInt>, _ s2: Span<CInt>) {
    mixedFuncWithSafeWrapper4(s, s2)
}

func callMixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ s2: Span<CInt>) {
    unsafe mixedFuncWithSafeWrapper5(s, s2)
}

func callMixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutableBufferPointer<CInt>) {
    unsafe mixedFuncWithSafeWrapper6(s, p)
}

func callMixedFuncWithSafeWrapper7(_ p: UnsafeBufferPointer<CInt>) {
    let _: ConstSpanOfInt = unsafe mixedFuncWithSafeWrapper7(p)
}

@lifetime(span: copy span)
func callMutableKeyword(_ span: inout MutableSpan<CInt>) {
    mutableKeyword(&span)
}

func callSpanWithoutTypeAlias(_ span: Span<CInt>) {
  spanWithoutTypeAlias(span) // expected-error {{cannot convert value of type}}
}

func callMutableSpanWithoutTypeAlias(_ span: consuming MutableSpan<CInt>) {
  mutableSpanWithoutTypeAlias(&span) // expected-error {{cannot convert value of type}}
}

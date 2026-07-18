// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++20 -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %S/Inputs/std-span.h --match-full-lines --implicit-check-not "@_alwaysEmitIntoClient" < %t/interface.swift

// Test legacy wrappers
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %S/Inputs -print-module -module-to-print=StdSpan -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature SafeInteropWrappers -Xcc -std=c++20 -module-cache-path %t > %t/interface-lifetimebound.swift
// RUN: %FileCheck %S/Inputs/std-span.h --match-full-lines --implicit-check-not "@_alwaysEmitIntoClient" --check-prefixes=CHECK,CHECK-LEGACY < %t/interface-lifetimebound.swift

// Make sure we trigger typechecking and SIL diagnostics
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -strict-memory-safety -verify -Xcc -std=c++20 %s -verify-additional-prefix default- -suppress-notes -eager-macro-checking

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -strict-memory-safety -verify -Xcc -std=c++20 %s -eager-macro-checking

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: std_span

import StdSpan
import CxxStdlib

func callMethodWithSafeWrapper(_ x: inout X, s: Span<CInt>) {
    x.methodWithSafeWrapper(s)
    let _ = x.getMutable(s) // expected-default-error {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'ConstSpanOfInt'}}
}

func callFooBar(_ x: inout SpanWithoutTypeAlias, _ s: ConstSpanOfInt) {
    let _: Span<CInt> = x.bar() // expected-default-error{{cannot convert value of type}}
    unsafe x.foo(s)
}

@_lifetime(span: copy span)
func callFuncWithMutableSafeWrapper(_ span: inout MutableSpan<CInt>, ) {
    FuncWithMutableSafeWrapper(&span)
}

@_lifetime(span: copy span)
func callFuncWithMutableSafeWrapper2(_ span: inout MutableSpan<CInt>, ) {
    // expected-default-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'SpanOfInt'}}
    // expected-default-error@+1{{cannot convert value of type 'SpanOfInt'}}
    let _: MutableSpan<CInt> = FuncWithMutableSafeWrapper2(&span)
}

@_lifetime(span: copy span)
func callMixedFuncWithMutableSafeWrapper1(_ span: inout MutableSpan<CInt>, ) {
    // expected-default-error@+3{{missing argument for parameter #2 in call}}
    // expected-default-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>' (aka 'UnsafeMutablePointer<MutableSpan<Int32>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
    // expected-default-error@+1{{cannot convert value of type 'SpanOfInt'}}
    let _: MutableSpan<CInt> = MixedFuncWithMutableSafeWrapper1(&span)
}

func MixedFuncWithMutableSafeWrapper2(_ v: VecOfInt) {
    var v2 = v
    // expected-default-error@+1{{cannot convert value of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    let _ : MutableSpan<CInt> = MixedFuncWithMutableSafeWrapper2(&v2, 37)
}

@_lifetime(span: copy span)
func callMixedFuncWithMutableSafeWrapper3(_ span: inout MutableSpan<CInt>, _ p: UnsafeMutableBufferPointer<CInt>) {
    unsafe MixedFuncWithMutableSafeWrapper3(&span, p)
}

@_lifetime(span1: copy span2)
@_lifetime(span2: copy span2)
func callMixedFuncWithMutableSafeWrapper4(_ span1: inout MutableSpan<CInt>, _ span2: inout MutableSpan<CInt>) {
    MixedFuncWithMutableSafeWrapper4(&span1, &span2)
}

@_lifetime(span: copy span)
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
    // expected-default-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'ConstSpanOfInt'}}
    let _ = funcWithSafeWrapper2(s)
}

func callFuncWithSafeWrapper3(_ v: borrowing VecOfInt) {
    // expected-default-error@+1{{cannot convert value of type 'ConstSpanOfInt'}}
    let _: Span<CInt> = funcWithSafeWrapper3(v)
}

func callMixedFuncWithSafeWrapper1(_ s: Span<CInt>) {
    // expected-default-error@+3{{missing argument for parameter #2 in call}}
    // expected-default-error@+2{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
    // expected-default-error@+1{{cannot convert value of type 'ConstSpanOfInt'}}
    let _: Span<CInt> = mixedFuncWithSafeWrapper1(s)
}

func callMixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt) {
    // expected-default-error@+1{{cannot convert value of type 'UnsafePointer<CInt>?' (aka 'Optional<UnsafePointer<Int32>>') to specified type 'Span<CInt>' (aka 'Span<Int32>')}}
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

@_lifetime(span: copy span)
func callMutableKeyword(_ span: inout MutableSpan<CInt>) {
    mutableKeyword(&span)
}

func callSpanWithoutTypeAlias(_ span: Span<CInt>) {
  spanWithoutTypeAlias(span)
}

func callMutableSpanWithoutTypeAlias(_ span: consuming MutableSpan<CInt>) {
  mutableSpanWithoutTypeAlias(&span)
}

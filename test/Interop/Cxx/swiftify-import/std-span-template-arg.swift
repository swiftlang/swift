// REQUIRES: std_span
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir \
// RUN:   -I %t -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature SafeInteropWrappers %t/test.swift \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note

//--- module.modulemap
module Test {
    header "test.h"
    requires cplusplus
    export *
}

//--- test.h
#pragma once
#include <span>

template <typename T>
struct S {
    // expected-expansion@+14:10{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload|}}
    //   expected-remark@3{{macro content: |public borrowing func get() -> Span<CChar> {|}}
    //   expected-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe get()), copying: ())|}}
    //   expected-remark@5{{macro content: |}|}}
    // }}
    // expected-expansion@+7:10{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload|}}
    //   expected-remark@3{{macro content: |public borrowing func get() -> Span<CInt> {|}}
    //   expected-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe get()), copying: ())|}}
    //   expected-remark@5{{macro content: |}|}}
    // }}
    T get() const [[clang::lifetimebound]];
    T getNoLifetime() const;
};

using SpanHolder = S<std::span<const char>>;
using IntSpan = std::span<const int>;
using UsingSpanHolder = S<IntSpan>;

template <typename T, typename U>
struct S2 {
    // expected-expansion@+7:11{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload|}}
    //   expected-remark@3{{macro content: |public borrowing func getT() -> Span<CChar> {|}}
    //   expected-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe getT()), copying: ())|}}
    //   expected-remark@5{{macro content: |}|}}
    // }}
    T getT() const [[clang::lifetimebound]];
    // expected-expansion@+7:11{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload|}}
    //   expected-remark@3{{macro content: |public borrowing func getU() -> Span<CInt> {|}}
    //   expected-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe getU()), copying: ())|}}
    //   expected-remark@5{{macro content: |}|}}
    // }}
    U getU() const [[clang::lifetimebound]];

    T getTPassUnrelatedU(U u) const [[clang::lifetimebound]];
};

using SpanHolder2 = S2<std::span<const char>, std::span<const int>>;

//--- test.swift
import Test

public func f(_ holder: SpanHolder) {
  let _ = holder.get();
  let _ = holder.getNoLifetime();
}
public func g(_ holder: UsingSpanHolder) {
  let _ = holder.get();
  let _ = holder.getNoLifetime();
}
public func h(_ holder: SpanHolder2, _ s: IntSpan) {
  let _ = holder.getT();
  let _ = holder.getU();
  let _ = holder.getTPassUnrelatedU(s);
}

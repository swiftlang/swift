// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: std_span

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/test.swift -emit-module -o %t/test.swiftmodule -Xcc -std=c++20
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/test.swift -emit-sil -Xcc -std=c++20 | %FileCheck %s

//--- Inputs/module.modulemap
module StdSpanNamespace {
    header "span.hpp"
    requires cplusplus
    export *
}

//--- Inputs/span.hpp
#pragma once

#include <span>
#include <lifetimebound.h>

using SpanFloat = std::span<const float>;
using MutableSpanFloat = std::span<float>;

namespace foo {

void takeSpan(SpanFloat buffer __noescape);
void takeMutableSpan(MutableSpanFloat buffer __noescape);

}

//--- Inputs/span.cpp
#include "span.hpp"

namespace foo {

void takeSpan(SpanFloat buffer __noescape) {
  return;
}

void takeMutableSpan(MutableSpanFloat buffer __noescape) {
  return;
}

}

//--- test.swift
import StdSpanNamespace

public func testBuffer(buffer: Span<Float>) {
  // CHECK: function_ref @{{.*}}foo{{.*}}takeSpan
  foo.takeSpan(buffer)
}

public func testBuffer2(buffer: inout MutableSpan<Float>) {
  // CHECK: function_ref @{{.*}}foo{{.*}}takeMutableSpan
  foo.takeMutableSpan(&buffer)
}

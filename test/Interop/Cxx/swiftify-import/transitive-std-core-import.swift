// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift  -cxx-interoperability-mode=default -Xcc -std=c++20

// Test that implicit imports can refer to std_core etc., even though Swift source code may not.

//--- Inputs/module.modulemap
module TestClang {
    header "test.h"
    requires cplusplus
    export std_core.cstddef.size_t
    export span
}

//--- Inputs/test.h

#include <__cstddef/size_t.h>
#include <span>
#include <lifetimebound.h>

using FloatSpan = std::span<const float>;

size_t foo(FloatSpan x __noescape);

//--- test.swift
import TestClang
  
func test(x: Span<Float>) {
  let _ = foo(x)
}


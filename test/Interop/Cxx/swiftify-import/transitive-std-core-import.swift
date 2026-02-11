// REQUIRES: swift_feature_StabilizedSafeInteropWrappers
// REQUIRES: OS=macosx

// Don't run this test with libc++ versions 17-19, when the top-level std module was split into multiple top-level modules.
// RUN: %empty-directory(%t)
// RUN: %target-clangxx %S/../stdlib/Inputs/check-libcxx-version.cpp -o %t/check-libcxx-version
// RUN: %target-codesign %t/check-libcxx-version

// RUN: %target-run %t/check-libcxx-version || split-file %s %t
// RUN: %target-run %t/check-libcxx-version || %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift  -cxx-interoperability-mode=default -Xcc -std=c++20

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


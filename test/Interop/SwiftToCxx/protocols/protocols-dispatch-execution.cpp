// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/protocols-dispatch.swift -module-name ProtoDispatch -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/proto-dispatch.h -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature CxxExistentialInterop

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/proto-dispatch-execution.o
// RUN: %target-interop-build-swift %S/Inputs/protocols-dispatch.swift -o %t/proto-dispatch-execution -Xlinker %t/proto-dispatch-execution.o -module-name ProtoDispatch -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/proto-dispatch-execution
// RUN: %target-run %t/proto-dispatch-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExistentialInterop

// End-to-end test: Swift functions that take and return protocol
// existentials, called from C++ via generated thunks.

#include <cassert>
#include <cstdio>
#include "proto-dispatch.h"

int main() {
    // Test 1: drawTwice(any Drawable) existential parameter.
    {
        auto circle = ProtoDispatch::Circle::init(7);
        ProtoDispatch::Drawable drawable(circle);
        swift::Int result = ProtoDispatch::drawTwice(drawable);
        printf("drawTwice() = %ld\n", result);
        assert(result == 98);  // 49 + 49
    }
// CHECK: drawTwice() = 98

    // Test 2: bestDrawable existential return picks the higher draw().
    {
        auto ca = ProtoDispatch::Circle::init(7);
        ProtoDispatch::Drawable a(ca);

        auto cb = ProtoDispatch::Circle::init(3);
        ProtoDispatch::Drawable b(cb);

        ProtoDispatch::Drawable best = ProtoDispatch::bestDrawable(a, b);
        swift::Int result = best.draw();
        printf("bestDrawable().draw() = %ld\n", result);
        assert(result == 49);  // a wins
    }
// CHECK-NEXT: bestDrawable().draw() = 49

    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

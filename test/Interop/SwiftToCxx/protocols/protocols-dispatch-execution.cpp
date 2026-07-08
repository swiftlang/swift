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

    // Test 3: makeDrawableAndResizable() composition return + method dispatch.
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        swift::Int drawResult = comp.draw();
        printf("comp.draw() = %ld\n", drawResult);
        assert(drawResult == 25);

        bool resizeResult = comp.resize(3);
        printf("comp.resize(3) = %s\n", resizeResult ? "true" : "false");
        assert(resizeResult == true);
    }
// CHECK-NEXT: comp.draw() = 25
// CHECK-NEXT: comp.resize(3) = true

    // Test 4: drawAndResize() composition parameter (derived-to-base).
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        swift::Int result = ProtoDispatch::drawAndResize(comp);
        printf("drawAndResize() = %ld\n", result);
        assert(result == 26);  // draw()=25 + resize(to:3)=true=1
    }
// CHECK-NEXT: drawAndResize() = 26

    // Test 5: asDrawable() extracts single-protocol Drawable from composition.
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        ProtoDispatch::Drawable drawable = comp.asDrawable();
        swift::Int result = drawable.draw();
        printf("asDrawable().draw() = %ld\n", result);
        assert(result == 25);
    }
// CHECK-NEXT: asDrawable().draw() = 25

    // Test 6: asResizable() extracts single-protocol Resizable from composition.
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        ProtoDispatch::Resizable resizable = comp.asResizable();
        bool result = resizable.resize(3);
        printf("asResizable().resize(3) = %s\n", result ? "true" : "false");
        assert(result == true);
    }
// CHECK-NEXT: asResizable().resize(3) = true

    // Test 7: Extracted Drawable passed to drawTwice() -- composition extraction
    // feeds single-protocol function dispatch.
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        ProtoDispatch::Drawable drawable = comp.asDrawable();
        swift::Int result = ProtoDispatch::drawTwice(drawable);
        printf("drawTwice(asDrawable()) = %ld\n", result);
        assert(result == 50);  // 25 + 25
    }
// CHECK-NEXT: drawTwice(asDrawable()) = 50

    // Test 8: Composition copy construction preserves both WTs.
    {
        auto comp = ProtoDispatch::makeDrawableAndResizable();
        auto copy(comp);
        printf("copy.draw() = %ld\n", copy.draw());
        printf("copy.resize(3) = %s\n", copy.resize(3) ? "true" : "false");
        assert(copy.draw() == 25);
        assert(copy.resize(3) == true);
    }
// CHECK-NEXT: copy.draw() = 25
// CHECK-NEXT: copy.resize(3) = true

    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

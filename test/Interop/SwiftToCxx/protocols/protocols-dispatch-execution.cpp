// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/protocols-dispatch.swift -module-name ProtoDispatch -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/proto-dispatch.h -cxx-interoperability-mode=upcoming-swift

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/proto-dispatch-execution.o
// RUN: %target-interop-build-swift %S/Inputs/protocols-dispatch.swift -o %t/proto-dispatch-execution -Xlinker %t/proto-dispatch-execution.o -module-name ProtoDispatch -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/proto-dispatch-execution
// RUN: %target-run %t/proto-dispatch-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// End-to-end test: protocol existential wrappers with boxing
// constructors, method dispatch, conversion, and VWT lifecycle.

#include <cassert>
#include <cstdio>
#include "proto-dispatch.h"

int main() {
    // Test 1: Drawable -- boxing constructor + draw().
    {
        auto circle = ProtoDispatch::Circle::init(7);
        ProtoDispatch::Drawable drawable(circle);
        swift::Int result = drawable.draw();
        printf("draw() = %ld\n", result);
        assert(result == 49);
    }
// CHECK: draw() = 49

    // Test 2: Resizable -- boxing constructor + resize(to:).
    {
        auto circle = ProtoDispatch::Circle::init(5);
        ProtoDispatch::Resizable resizable(circle);
        bool ok = resizable.resize(3);
        printf("resize(3) = %s\n", ok ? "true" : "false");
        assert(ok == true);

        bool bad = resizable.resize(10);
        printf("resize(10) = %s\n", bad ? "true" : "false");
        assert(bad == false);
    }
// CHECK-NEXT: resize(3) = true
// CHECK-NEXT: resize(10) = false

    // Test 3: Stylable -- inherited draw() via two-level WT dispatch,
    // plus own style() via direct dispatch.
    {
        auto sc = ProtoDispatch::StyledCircle::init(7);
        ProtoDispatch::Stylable stylable(sc);

        swift::Int drawResult = stylable.draw();
        printf("stylable.draw() = %ld\n", drawResult);
        assert(drawResult == 147);  // 7*7*3

        bool styleResult = stylable.style();
        printf("stylable.style() = %s\n", styleResult ? "true" : "false");
        assert(styleResult == true);  // 7 > 5
    }
// CHECK-NEXT: stylable.draw() = 147
// CHECK-NEXT: stylable.style() = true

    // Test 4: Conversion -- Stylable::asDrawable() copies the
    // existential into a Drawable wrapper with the base WT.
    {
        auto sc = ProtoDispatch::StyledCircle::init(7);
        ProtoDispatch::Stylable stylable(sc);

        ProtoDispatch::Drawable drawable = stylable.asDrawable();
        swift::Int drawResult = drawable.draw();
        printf("asDrawable().draw() = %ld\n", drawResult);
        assert(drawResult == 147);
    }
// CHECK-NEXT: asDrawable().draw() = 147

    // Test 5: Container<swift::Int> -- PAT class template via boxing.
    {
        auto arr = ProtoDispatch::IntArray::init(5);
        ProtoDispatch::Container<swift::Int> container(arr);
        swift::Int n = container.count();
        printf("container.count() = %ld\n", n);
        assert(n == 5);
    }
// CHECK-NEXT: container.count() = 5

    // Test 6: Existential parameter -- drawTwice(any Drawable).
    {
        auto circle = ProtoDispatch::Circle::init(7);
        ProtoDispatch::Drawable drawable(circle);
        swift::Int result = ProtoDispatch::drawTwice(drawable);
        printf("drawTwice() = %ld\n", result);
        assert(result == 98);  // 49 + 49
    }
// CHECK-NEXT: drawTwice() = 98

    // Test 7: Existential return -- bestDrawable picks the higher draw().
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

    // Test 8: VWT copy construction.
    {
        auto circle = ProtoDispatch::Circle::init(5);
        ProtoDispatch::Drawable original(circle);
        ProtoDispatch::Drawable copy(original);
        swift::Int result = copy.draw();
        printf("copy.draw() = %ld\n", result);
        assert(result == 25);
    }
// CHECK-NEXT: copy.draw() = 25

    // Test 9: VWT copy assignment.
    {
        auto c1 = ProtoDispatch::Circle::init(5);
        ProtoDispatch::Drawable assigned(c1);
        printf("before assign: %ld\n", assigned.draw());

        auto c2 = ProtoDispatch::Circle::init(3);
        ProtoDispatch::Drawable other(c2);
        assigned = other;
        printf("after assign: %ld\n", assigned.draw());
        assert(assigned.draw() == 9);
    }
// CHECK-NEXT: before assign: 25
// CHECK-NEXT: after assign: 9

    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

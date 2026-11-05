// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/protocols-dispatch-base.swift -module-name ProtoDispatch -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/proto-dispatch.h -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature CxxExistentialInterop

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/proto-dispatch-execution.o
// RUN: %target-interop-build-swift %S/Inputs/protocols-dispatch-base.swift -o %t/proto-dispatch-execution -Xlinker %t/proto-dispatch-execution.o -module-name ProtoDispatch -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/proto-dispatch-execution
// RUN: %target-run %t/proto-dispatch-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExistentialInterop

// End-to-end test: protocol existential wrappers with boxing
// constructors, method dispatch, conversion, and VWT lifecycle.

#include <cassert>
#include <cstdio>
#include "proto-dispatch.h"

int main() {
    // Test 1: Drawable boxing constructor + draw().
    {
        auto circle = ProtoDispatch::Circle::init(7);
        ProtoDispatch::Drawable drawable(circle);
        swift::Int result = drawable.draw();
        printf("draw() = %ld\n", result);
        assert(result == 49);
    }
// CHECK: draw() = 49

    // Test 2: Resizable boxing constructor + resize(to:).
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

    // Test 3: Stylable inherited draw() via two-level WT dispatch,
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

    // Test 4: Stylable::asDrawable() copies the
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

    // Test 5: VWT copy construction (opaque existential).
    {
        auto circle = ProtoDispatch::Circle::init(5);
        ProtoDispatch::Drawable original(circle);
        ProtoDispatch::Drawable copy(original);
        swift::Int result = copy.draw();
        printf("copy.draw() = %ld\n", result);
        assert(result == 25);
    }
// CHECK-NEXT: copy.draw() = 25

    // Test 6: VWT copy assignment.
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

    // Test 7: Move construction. Value is transferred, source is
    // left in a moved-from state (opaque existential).
    {
        auto circle = ProtoDispatch::Circle::init(6);
        ProtoDispatch::Drawable original(circle);
        ProtoDispatch::Drawable moved(std::move(original));
        swift::Int result = moved.draw();
        printf("moved.draw() = %ld\n", result);
        assert(result == 36);
    }
// CHECK-NEXT: moved.draw() = 36

    // Test 8: Move assignment (opaque existential).
    {
        auto c1 = ProtoDispatch::Circle::init(4);
        ProtoDispatch::Drawable target(c1);

        auto c2 = ProtoDispatch::Circle::init(8);
        ProtoDispatch::Drawable source(c2);
        target = std::move(source);
        printf("move-assigned.draw() = %ld\n", target.draw());
        assert(target.draw() == 64);
    }
// CHECK-NEXT: move-assigned.draw() = 64

    // Test 9: Class-bound protocol boxing, dispatch, copy, move.
    {
        auto canvas = ProtoDispatch::Canvas::init(21);
        ProtoDispatch::Renderable renderable(canvas);
        swift::Int result = renderable.render();
        printf("renderable.render() = %ld\n", result);
        assert(result == 42);

        ProtoDispatch::Renderable copy(renderable);
        printf("copy.render() = %ld\n", copy.render());
        assert(copy.render() == 42);

        ProtoDispatch::Renderable moved(std::move(copy));
        printf("moved.render() = %ld\n", moved.render());
        assert(moved.render() == 42);

        auto canvas2 = ProtoDispatch::Canvas::init(50);
        ProtoDispatch::Renderable r2(canvas2);
        r2 = renderable;
        printf("copy-assigned.render() = %ld\n", r2.render());
        assert(r2.render() == 42);

        auto canvas3 = ProtoDispatch::Canvas::init(99);
        ProtoDispatch::Renderable r3(canvas3);
        r3 = std::move(renderable);
        printf("move-assigned.render() = %ld\n", r3.render());
        assert(r3.render() == 42);
    }
// CHECK-NEXT: renderable.render() = 42
// CHECK-NEXT: copy.render() = 42
// CHECK-NEXT: moved.render() = 42
// CHECK-NEXT: copy-assigned.render() = 42
// CHECK-NEXT: move-assigned.render() = 42

    // Test 10: Multi-requirement protocol at offsets 1 and 2.
    {
        auto pair = ProtoDispatch::Pair::init(10, 20);
        ProtoDispatch::MultiReq mr(pair);
        printf("first() = %ld\n", mr.first());
        printf("second() = %ld\n", mr.second());
        assert(mr.first() == 10);
        assert(mr.second() == 20);
    }
// CHECK-NEXT: first() = 10
// CHECK-NEXT: second() = 20

    // Test 11: Large value type exceeds inline buffer (4 words > 3-word buffer),
    // exercises VWT outline storage (swift_allocBox / swift_projectBox).
    {
        auto large = ProtoDispatch::LargeDrawable::init(1, 2, 3, 4);
        ProtoDispatch::Drawable drawable(large);
        swift::Int result = drawable.draw();
        printf("large.draw() = %ld\n", result);
        assert(result == 10);

        ProtoDispatch::Drawable copy(drawable);
        printf("large copy.draw() = %ld\n", copy.draw());
        assert(copy.draw() == 10);

        ProtoDispatch::Drawable moved(std::move(drawable));
        printf("large moved.draw() = %ld\n", moved.draw());
        assert(moved.draw() == 10);
    }
// CHECK-NEXT: large.draw() = 10
// CHECK-NEXT: large copy.draw() = 10
// CHECK-NEXT: large moved.draw() = 10

    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

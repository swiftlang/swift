// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/existential-poc.swift -module-name ExistentialPOC -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/existential-poc.h -cxx-interoperability-mode=upcoming-swift

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/existential-poc-execution.o
// RUN: %target-interop-build-swift %S/Inputs/existential-poc.swift -o %t/existential-poc-execution -Xlinker %t/existential-poc-execution.o -module-name ExistentialPOC -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/existential-poc-execution
// RUN: %target-run %t/existential-poc-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// This test is a proof-of-concept for protocol existential wrappers.
// Everything here is hand-written scaffolding that validates the
// SwiftExistentialType base class. None of the stubs below -- the
// @_cdecl thunks, the DrawableExistential subclass, or the _make
// factories -- will appear in the real implementation. The compiler
// (PrintAsClang) will generate all of this automatically:
//
//   - extern "C" thunks for existential construction and protocol
//     requirement dispatch (like it already does for class methods)
//   - A per-protocol subclass of SwiftExistentialType with a WT
//     pointer and method stubs calling those thunks
//   - _impl factory functions wiring the thunks to the subclass
//
// What this test validates is the base class machinery: that the
// existential container layout matches between Swift and C++, and
// that VWT-delegated copy/move/destroy works correctly.

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <new>
#include "existential-poc.h"

// --- Stubs: @_cdecl thunks standing in for compiler-generated ones ---

extern "C" void createCircleDrawable(void *out, swift::Int radius);
extern "C" void createSquareDrawable(void *out, swift::Int side);
extern "C" swift::Int drawableCallDraw(const void *existential);
extern "C" long getDrawableExistentialSize();
extern "C" long getDrawableExistentialAlignment();

// --- Stub: hand-written subclass standing in for compiler-generated wrapper ---

struct DrawableExistential : swift::_impl::SwiftExistentialType {

    swift::Int draw() const {
        return drawableCallDraw(this);
    }

    static DrawableExistential _make(swift::Int radius) {
        DrawableExistential result;
        createCircleDrawable(&result, radius);
        return result;
    }

    static DrawableExistential _makeSquare(swift::Int side) {
        DrawableExistential result;
        createSquareDrawable(&result, side);
        return result;
    }

private:
    DrawableExistential() : SwiftExistentialType(uninit_t{}) {}

    const void *_Nonnull _drawWT;

    friend struct _impl_DrawableExistential;
};

int main() {
    // Verify layout match between Swift and C++.
    long swiftSize = getDrawableExistentialSize();
    long swiftAlign = getDrawableExistentialAlignment();
    printf("Swift existential: size=%ld align=%ld\n", swiftSize, swiftAlign);
    printf("C++ wrapper:       size=%zu align=%zu\n",
           sizeof(DrawableExistential), alignof(DrawableExistential));
    assert(sizeof(DrawableExistential) == (size_t)swiftSize);
    assert(alignof(DrawableExistential) == (size_t)swiftAlign);
// CHECK: Swift existential: size=40 align=8
// CHECK-NEXT: C++ wrapper:       size=40 align=8

    // Create a Circle(radius: 5) existential via factory.
    auto circle = DrawableExistential::_make(5);
    swift::Int r1 = circle.draw();
    printf("circle.draw() = %ld\n", r1);
    assert(r1 == 25);
// CHECK-NEXT: circle.draw() = 25

    // Create a Square(side: 3) existential.
    auto square = DrawableExistential::_makeSquare(3);
    swift::Int r2 = square.draw();
    printf("square.draw() = %ld\n", r2);
    assert(r2 == 36);
// CHECK-NEXT: square.draw() = 36

    // Test VWT-based copy construction.
    {
        DrawableExistential circleCopy(circle);
        swift::Int r3 = circleCopy.draw();
        printf("circleCopy.draw() = %ld\n", r3);
        assert(r3 == 25);
        // circleCopy destructor runs here -- VWT destroy
    }
// CHECK-NEXT: circleCopy.draw() = 25

    // Test VWT-based copy assignment.
    {
        auto assigned = DrawableExistential::_make(1);
        printf("before assign: %ld\n", assigned.draw());
        assigned = square;
        printf("after assign:  %ld\n", assigned.draw());
        assert(assigned.draw() == 36);
        // assigned destructor runs here
    }
// CHECK-NEXT: before assign: 1
// CHECK-NEXT: after assign:  36

    // Original circle and square destructors run here.
    // If VWT destroy is broken, ASAN/LeakSanitizer would catch it.
    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

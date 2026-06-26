// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/protocols-dispatch.swift -module-name ProtoDispatch -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/proto-dispatch.h -cxx-interoperability-mode=upcoming-swift

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/proto-dispatch-execution.o
// RUN: %target-interop-build-swift %S/Inputs/protocols-dispatch.swift -o %t/proto-dispatch-execution -Xlinker %t/proto-dispatch-execution.o -module-name ProtoDispatch -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/proto-dispatch-execution
// RUN: %target-run %t/proto-dispatch-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// End-to-end test: compiler-generated protocol wrapper methods
// dispatch through the real witness table via _callWitness.

#include <cassert>
#include <cstdio>
#include "proto-dispatch.h"

extern "C" void createCircleDrawable(void *out, swift::Int radius);
extern "C" void createCircleResizable(void *out, swift::Int radius);

int main() {
    // Test 1: Drawable::draw() -- no user params, offset 1.
    {
        // Create uninitialized storage matching the wrapper layout.
        alignas(alignof(ProtoDispatch::Drawable))
            char storage[sizeof(ProtoDispatch::Drawable)];
        createCircleDrawable(storage, 7);

        auto &drawable =
            *reinterpret_cast<ProtoDispatch::Drawable *>(storage);
        swift::Int result = drawable.draw();
        printf("draw() = %ld\n", result);
        assert(result == 49);
    }
// CHECK: draw() = 49

    // Test 2: Resizable::resize(to:) -- one Int param, tests the
    // _callWitness parameter ordering (user args before self/metadata/wt).
    {
        alignas(alignof(ProtoDispatch::Resizable))
            char storage[sizeof(ProtoDispatch::Resizable)];
        createCircleResizable(storage, 5);

        auto &resizable =
            *reinterpret_cast<ProtoDispatch::Resizable *>(storage);
        bool ok = resizable.resize(3);
        printf("resize(3) = %s\n", ok ? "true" : "false");
        assert(ok == true);

        bool bad = resizable.resize(10);
        printf("resize(10) = %s\n", bad ? "true" : "false");
        assert(bad == false);
    }
// CHECK-NEXT: resize(3) = true
// CHECK-NEXT: resize(10) = false

    printf("done\n");
// CHECK-NEXT: done
    return 0;
}

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/small-enums-pass-return-in-cxx.swift -module-name Enums -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/small-enums-pass-return-in-cxx.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include <cstddef>
#include "enums.h"

int main() {
    using namespace Enums;

    // sizeof(generated cxx class) = 1 + max(sizeof(case) for all cases) + padding
    static_assert(sizeof(Tiny) == 1, "MemoryLayout<Tiny>.stride == 1");
    static_assert(sizeof(Small) == 16, "MemoryLayout<Small>.stride == 16");

    auto tiny = makeTiny(10);
    printTiny(tiny);
    // CHECK: Tiny.first
    inoutTiny(tiny, -1);
    printTiny(tiny);
    // CHECK: Tiny.second
    printTiny(passThroughTiny(tiny));
    // CHECK: Tiny.second

    auto small = makeSmall(-3);
    printSmall(small);
    // CHECK: Small.second(3.0)
    inoutSmall(small, 100);
    printSmall(small);
    // CHECK: Small.first(100)
    printSmall(passThroughSmall(small));
    // CHECK: Small.first(100)

    return 0;
}

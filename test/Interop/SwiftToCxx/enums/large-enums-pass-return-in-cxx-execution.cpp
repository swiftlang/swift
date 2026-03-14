// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/large-enums-pass-return-in-cxx.swift -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/large-enums-pass-return-in-cxx.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include <cstdint>
#include "enums.h"

int main() {
    using namespace Enums;

    // sizeof(generated cxx class) = 1 + max(sizeof(case) for all cases) + padding
    static_assert(sizeof(Large) == 56, "MemoryLayout<Large>.stride == 56");

    auto large = makeLarge(-1);
    printLarge(large);
    // CHECK: Large.second
    inoutLarge(large, 10);
    printLarge(large);
    // CHECK: Large.first(-1, -2, -3, -4, -5, -6)
    printLarge(passThroughLarge(large));
    // CHECK: Large.first(-1, -2, -3, -4, -5, -6)

    return 0;
}

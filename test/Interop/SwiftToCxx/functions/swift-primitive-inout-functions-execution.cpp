// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-primitive-inout-functions-cxx-bridging.swift -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-primitive-inout-functions-cxx-bridging.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

#define VERIFY_INOUT_VALUE(FUNC, TYPENAME, INITIAL_VALUE, EXPECT_VALUE) \
do { \
    TYPENAME variable = INITIAL_VALUE; \
    FUNC(variable); \
    assert(variable == EXPECT_VALUE); \
} while (false);

int main() {
    using namespace Functions;

    VERIFY_INOUT_VALUE(inOutInt, swift::Int, swift::Int{1}, swift::Int{0});

    {
        swift::Int x{1}, y{2};
        inOutTwoInt(x, y);
        assert(x == swift::Int{3});
        assert(y == swift::Int{-4});
    }

    {
        bool x = false;
        double y = 6.28;
        inOutTwoParam(x, y);
        assert(x);
        assert(y == 3.14);
    }
}

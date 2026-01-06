// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-transparent-functions-cxx-bridging.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-transparent-functions-cxx-bridging.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

int main() {
    using namespace Functions;
    
    assert(transparentPrimitiveFunc(12) == 144);

    return 0;
}

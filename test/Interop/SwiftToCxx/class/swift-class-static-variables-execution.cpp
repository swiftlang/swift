// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-class-static-variables.swift -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o
// RUN: %target-interop-build-swift %S/swift-class-static-variables.swift -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -module-name Class -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution

// REQUIRES: executable_test


#include "class.h"
#include <assert.h>
#include <cstdio>

using namespace Class;

int main() {
    auto x = FileUtilities::getShared();
    assert(x.getField() == 42);
}

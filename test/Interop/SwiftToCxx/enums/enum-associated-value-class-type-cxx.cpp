// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/enum-associated-value-class-type-cxx.swift -typecheck -module-name Enums -clang-header-expose-decls=all-public -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/enum-associated-value-class-type-cxx.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

int main() {
    auto c = C::init(1234);
    assert(c.getX() == 1234);

    auto e1 = E::c(c);
    assert(e1.isC());
    assert(e1.getC().getX() == 1234);

    auto e2 = E::i(5678);
    assert(e2.isI());
    assert(e2.getI() == 5678);
    return 0;
}

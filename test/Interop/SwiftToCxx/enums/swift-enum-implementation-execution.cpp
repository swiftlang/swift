// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-enum-implementation.swift -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/swift-enum-implementation.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

void switchTest(const E &e) {
    switch (e) {
    case E::x:
        assert(e.isX());
        assert(e.getX() == 3.14);
        break;
    case E::y:
        assert(e.isY());
        assert(e.getY() == nullptr);
        break;
    case E::z:
        assert(e.isZ());
        assert(e.getZ().getX() == 1234);
        break;
    case E::w:
        assert(e.isW());
        assert(e.getW() == 5678);
        break;
    case E::auto_:
        assert(e.isAuto_());
        assert(e.getAuto_() == reinterpret_cast<void *>(1));
        break;
    case E::foobar:
        assert(e.isFoobar());
        break;
    }
}

int main() {
    {
        auto e = E::x(3.14);
        switchTest(e);
    }

    {
        auto e = E::y(nullptr);
        switchTest(e);
    }

    {
        auto e = E::z(S::init(1234));
        switchTest(e);
    }

    {
        auto e = E::w(5678);
        switchTest(e);
    }

    {
        auto e = E::auto_(reinterpret_cast<void *>(1));
        switchTest(e);
    }

    {
        auto e = E::foobar();
        switchTest(e);
    }
    return  0;
}

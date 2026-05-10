// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-enum-implementation.swift -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/swift-enum-implementation.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

int switchTest(const E &e) {
    switch (e) {
    case E::x:
        assert(e.isX());
        assert(e.getX() == 3.14);
        return 0;
    case E::y:
        assert(e.isY());
        assert(e.getY() == nullptr);
        return 1;
    case E::z:
        assert(e.isZ());
        assert(e.getZ().getX() == 1234);
        return 2;
    case E::w:
        assert(e.isW());
        assert(e.getW() == 5678);
        return 3;
    case E::auto_:
        assert(e.isAuto_());
        assert(e.getAuto_() == reinterpret_cast<void *>(1));
        return 4;
    case E::foobar:
        assert(e.isFoobar());
        return 5;
    }
}

int main() {
    {
        auto e = E::x(3.14);
        assert(switchTest(e) == 0);
    }

    {
        auto e = E::y(nullptr);
        assert(switchTest(e) == 1);
    }

    {
        auto e = E::z(S::init(1234));
        assert(switchTest(e) == 2);
    }

    {
        auto e = E::w(5678);
        assert(switchTest(e) == 3);
    }

    {
        auto e = E::auto_(reinterpret_cast<void *>(1));
        assert(switchTest(e) == 4);
    }

    {
        auto e = E::foobar();
        assert(switchTest(e) == 5);
    }

    {
        auto e = E::init();
        assert(switchTest(e) == 5);
    }

    {
        auto e = E::init();
        assert(e.getTen() == 10);
        e.printSelf();
    }
// CHECK: self
    return  0;
}

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-enum-case-functions.swift -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/swift-enum-case-functions.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

void cxxCheckEnum(const DataCase &e) {
    assert(e.isOne());
}

void cxxCheckEnum(const CLikeEnum &e) {
    switch (e) {
        case CLikeEnum::cases::one:
            assert(checkCLikeEnum(e, 1));
            assert(e.isOne());
            break;
        case CLikeEnum::cases::two:
            assert(checkCLikeEnum(e, 2));
            assert(e.isTwo());
            break;
        case CLikeEnum::cases::three:
            assert(checkCLikeEnum(e, 3));
            assert(e.isThree());
            break;
    }
}

void cxxCheckEnum(const BoolWithCase &e) {
    switch (e) {
        case BoolWithCase::cases::first:
            assert(checkBoolWithCase(e, 1));
            assert(e.isFirst());
            break;
        case BoolWithCase::cases::second:
            assert(checkBoolWithCase(e, 2));
            assert(e.isSecond());
            break;
        case BoolWithCase::cases::third:
            assert(checkBoolWithCase(e, 3));
            assert(e.isThird());
            break;
    }
}

void cxxCheckEnum(const IntOrInfinity &e) {
    switch (e) {
        case IntOrInfinity::cases::NegInfinity:
            assert(checkIntOrInfinity(e, 1));
            assert(e.isNegInfinity());
            break;
        case IntOrInfinity::cases::Int:
            assert(checkIntOrInfinity(e, 2));
            assert(e.isInt());
            break;
        case IntOrInfinity::cases::PosInfinity:
            assert(checkIntOrInfinity(e, 3));
            assert(e.isPosInfinity());
            break;
    }
}

void cxxCheckEnum(const MultipleBoolWithCase &e) {
    switch (e) {
        case MultipleBoolWithCase::cases::first:
            assert(checkMultipleBoolWithCase(e, 1));
            assert(e.isFirst());
            break;
        case MultipleBoolWithCase::cases::second:
            assert(checkMultipleBoolWithCase(e, 2));
            assert(e.isSecond());
            break;
        case MultipleBoolWithCase::cases::third:
            assert(checkMultipleBoolWithCase(e, 3));
            assert(e.isThird());
            break;
        case MultipleBoolWithCase::cases::fourth:
            assert(checkMultipleBoolWithCase(e, 4));
            assert(e.isFourth());
            break;
    }
}

void cxxCheckEnum(const IntDoubleOrBignum &e) {
    switch (e) {
        case IntDoubleOrBignum::cases::Int:
            assert(checkIntDoubleOrBignum(e, 1));
            assert(e.isInt());
            break;
        case IntDoubleOrBignum::cases::Double:
            assert(checkIntDoubleOrBignum(e, 2));
            assert(e.isDouble());
            break;
        case IntDoubleOrBignum::cases::Bignum:
            assert(checkIntDoubleOrBignum(e, 3));
            assert(e.isBignum());
            break;
    }
}

int main() {
    {
        auto e1 = makeDataCase();
        cxxCheckEnum(e1);
    }

    {
        auto e1 = makeCLikeEnum(1);
        auto e2 = makeCLikeEnum(2);
        auto e3 = makeCLikeEnum(3);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
    }

    {
        auto e1 = makeBoolWithCase(1);
        auto e2 = makeBoolWithCase(2);
        auto e3 = makeBoolWithCase(3);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
    }

    {
        auto e1 = makeIntOrInfinity(1);
        auto e2 = makeIntOrInfinity(2);
        auto e3 = makeIntOrInfinity(3);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
    }

    {
        auto e1 = makeMultipleBoolWithCase(1);
        auto e2 = makeMultipleBoolWithCase(2);
        auto e3 = makeMultipleBoolWithCase(3);
        auto e4 = makeMultipleBoolWithCase(4);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
        cxxCheckEnum(e4);
    }

    {
        auto e1 = makeIntDoubleOrBignum(1);
        auto e2 = makeIntDoubleOrBignum(2);
        auto e3 = makeIntDoubleOrBignum(3);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
    }
    return  0;
}

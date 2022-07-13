// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-enum-case-functions.swift -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/swift-enum-case-functions.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// REQUIRES: executable_test
// UNSUPPORTED: CPU=arm64e

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

void cxxCheckEnum(const CharOrSectionMarker &e) {
    switch (e) {
        case CharOrSectionMarker::cases::Paragraph:
            assert(checkCharOrSectionMarker(e, 1));
            assert(e.isParagraph());
            break;
        case CharOrSectionMarker::cases::Char:
            assert(checkCharOrSectionMarker(e, 2));
            assert(e.isChar());
            break;
        case CharOrSectionMarker::cases::Chapter:
            assert(checkCharOrSectionMarker(e, 3));
            assert(e.isChapter());
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

void cxxCheckEnum(const TerminalChar &e) {
    switch (e) {
        case TerminalChar::cases::Plain:
            assert(checkTerminalChar(e, 1));
            assert(e.isPlain());
            break;
        case TerminalChar::cases::Bold:
            assert(checkTerminalChar(e, 2));
            assert(e.isBold());
            break;
        case TerminalChar::cases::Underline:
            assert(checkTerminalChar(e, 3));
            assert(e.isUnderline());
            break;
        case TerminalChar::cases::Blink:
            assert(checkTerminalChar(e, 4));
            assert(e.isBlink());
            break;
        case TerminalChar::cases::Empty:
            assert(checkTerminalChar(e, 5));
            assert(e.isEmpty());
            break;
        case TerminalChar::cases::Cursor:
            assert(checkTerminalChar(e, 6));
            assert(e.isCursor());
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
        auto e1 = makeCharOrSectionMarker(1);
        auto e2 = makeCharOrSectionMarker(2);
        auto e3 = makeCharOrSectionMarker(3);

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
        auto e1 = makeTerminalChar(1);
        auto e2 = makeTerminalChar(2);
        auto e3 = makeTerminalChar(3);
        auto e4 = makeTerminalChar(4);
        auto e5 = makeTerminalChar(5);
        auto e6 = makeTerminalChar(6);

        cxxCheckEnum(e1);
        cxxCheckEnum(e2);
        cxxCheckEnum(e3);
        cxxCheckEnum(e4);
        cxxCheckEnum(e5);
        cxxCheckEnum(e6);
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

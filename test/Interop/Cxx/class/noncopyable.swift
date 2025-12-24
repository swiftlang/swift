// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift %t%{fs-sep}test.swift -I %t%{fs-sep}Inputs -o %t%{fs-sep}out -cxx-interoperability-mode=default
// RUN: %target-codesign %t%{fs-sep}out
// RUN: %target-run %t%{fs-sep}out
//
// REQUIRES: executable_test

//--- Inputs/module.modulemap
module Test {
    header "noncopyable.h"
    requires cplusplus
}

//--- Inputs/noncopyable.h
#include <string>

struct NonCopyable {
    NonCopyable() = default;
    NonCopyable(int x) : number(x) {}
    NonCopyable(const NonCopyable& other) = delete; 
    NonCopyable(NonCopyable&& other) = default;

    int number = 0;
};

template<typename T>
struct HasSubscript {
    T &operator[](int idx) { return element; }
    T element;
};

using HasSubscriptInt = HasSubscript<int>;
using HasSubscriptNonCopyable = HasSubscript<NonCopyable>;

struct InheritsFromHasSubscript : public HasSubscript<NonCopyable> {};

//--- test.swift
import StdlibUnittest
import Test

var NonCopyableTestSuite = TestSuite("NonCopyable")

func borrow(_ x: borrowing NonCopyable) -> Int32 { return x.number; }

NonCopyableTestSuite.test("Use subscript") {
    var o1 = HasSubscriptInt(element: 7)
    expectEqual(o1[42], 7)

    var o2 = HasSubscriptNonCopyable(element: NonCopyable(5))
    expectEqual(borrow(o2[42]), 5)

    var inherited = InheritsFromHasSubscript()
    expectEqual(borrow(inherited[42]), 0)
}

NonCopyableTestSuite.test("Mutate subscript") {
    var o1 = HasSubscriptInt(element: 7)
    expectEqual(o1[42], 7)
    o1[42] = 8
    expectEqual(o1[42], 8)

    var o2 = HasSubscriptNonCopyable(element: NonCopyable(5))
    expectEqual(borrow(o2[42]), 5)
    o2[42] = NonCopyable(16)
    expectEqual(borrow(o2[42]), 16)

    var inherited = InheritsFromHasSubscript()
    expectEqual(borrow(inherited[42]), 0)
    inherited[42] = NonCopyable(3)
    expectEqual(borrow(inherited[42]), 3)
}

runAllTests()

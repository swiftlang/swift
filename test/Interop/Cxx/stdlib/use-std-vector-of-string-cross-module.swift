// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift %t/test.swift -I %t/Inputs -o %t/out -cxx-interoperability-mode=swift-6
// RUN: %target-codesign %t/out
// RUN: %target-run %t/out

// REQUIRES: executable_test

//--- Inputs/module.modulemap
module VectorOfStringLib {
    header "vector-of-string-lib.h"
    requires cplusplus
    export *
}

//--- Inputs/vector-of-string-lib.h
#ifndef VECTOR_OF_STRING_LIB_H
#define VECTOR_OF_STRING_LIB_H

#include <string>
#include <vector>

using VectorOfString = std::vector<std::string>;

inline VectorOfString makeVectorOfString() {
    return {"hello", "world", "test"};
}

inline void triggerCrash() {
    VectorOfString::const_iterator a;
    // Used to crash when std::vector<std::string> is imported from a
    // C++ module whose code triggers instantiation of const_iterator::operator-,
    (void)(a - a); // This was crucial for the crash.
}

#endif

//--- test.swift
import StdlibUnittest
import VectorOfStringLib
import CxxStdlib

var Suite = TestSuite("VectorOfStringCrossModule")

Suite.test("map on vector<string> from push_back") {
    var v = VectorOfString()
    v.push_back(std.string("abc"))
    v.push_back(std.string("a"))
    v.push_back(std.string("ab"))

    let lengths = v.map { $0.length() }
    expectEqual(lengths, [3, 1, 2])
}

Suite.test("map on vector<string> from factory function") {
    let v = makeVectorOfString()

    let lengths = v.map { $0.length() }
    expectEqual(lengths, [5, 5, 4])
}

Suite.test("for loop over vector<string>") {
    let v = makeVectorOfString()
    var count = 0
    for s in v {
        count += s.length()
    }
    expectEqual(count, 14)
}

runAllTests()

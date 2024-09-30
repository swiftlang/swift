// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/swiftMod.swift -typecheck -module-name SwiftMod -emit-clang-header-path %t/swiftMod.h -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS

// RUN: %target-interop-build-swift %t/swiftMod.swift -o %t/swift-execution -module-name SwiftMod -I %t -g -DSECOND_PASS -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

// RUN: %target-codesign %t/swift-execution
// RUN: %target-run %t/swift-execution | %FileCheck %s

// REQUIRES: executable_test

//--- header.h
#ifndef FIRSTPASS

#include "swiftMod.h"

inline swift::String createString() {
    return swift::String("Foobar");
}

inline swift::Optional<swift::String> createOptionalString() {
    return swift::Optional<swift::String>::some("Bar");
}

#endif

//--- module.modulemap
module SwiftToCxxTest {
    header "header.h"
    requires cplusplus
}

//--- swiftMod.swift
import SwiftToCxxTest

public func f() -> String? { "" }

#if SECOND_PASS

let str = createString()
print(str)
let opt = createOptionalString()
print(opt)

#endif

// CHECK: Foobar

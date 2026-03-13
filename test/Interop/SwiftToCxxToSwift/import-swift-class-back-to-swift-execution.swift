// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/swiftMod.swift -module-name SwiftMod -typecheck -verify -emit-clang-header-path %t/swiftMod.h -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS

// RUN: %target-interop-build-swift %t/swiftMod.swift -o %t/swift-execution -module-name SwiftMod -I %t -g -DSECOND_PASS -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

// RUN: %target-codesign %t/swift-execution
// RUN: %target-run %t/swift-execution | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: OS=windows-msvc

//--- header.h
#ifndef FIRSTPASS

#include "swiftMod.h"

inline SwiftMod::ExposedToCxx createSwiftClassInCxx() {
    return SwiftMod::ExposedToCxx::init();
}

inline void passSwiftClassToCxx(SwiftMod::ExposedToCxx value) {
    value.setI(-11);
    value.testMethod();
}

// FIXME: Add test for `moved` passThrough once 'move' is supported
// for Swift class typesin C++.
inline SwiftMod::ExposedToCxx passThroughSwiftClass(SwiftMod::ExposedToCxx value) {
    auto copy = value;
    value.setI(2);
    return copy;
}

int puts(const char *);

// FIXME: There's a bug where destructor isn't called if
// it's not explicitly defined.
class __attribute__((swift_attr("import_owned"))) InClass {
    SwiftMod::ExposedToCxx value;
public:
    ~InClass() {
        puts("destroy ~InClass()");
    }
    inline InClass(SwiftMod::ExposedToCxx value) : value(value) {}

    inline SwiftMod::ExposedToCxx getValue() const {
        return value;
    }
};

#endif

//--- module.modulemap
module SwiftToCxxTest {
    header "header.h"
    requires cplusplus
}

//--- swiftMod.swift
import SwiftToCxxTest

public class ExposedToCxx {
    public init() {
        i = 0
        print("ExposedToCxx.init")
    }
    deinit {
        print("ExposedToCxx\(i).deinit")
    }

    public final func testMethod() {
        print("ExposedToCxx\(i).testMethod")
    }

    public var i: Int
}

#if SECOND_PASS

func testReceiveAndPassSwiftClass() {
    let classInstance = createSwiftClassInCxx()
    classInstance.testMethod()
    classInstance.i = 12
    classInstance.testMethod()
    passSwiftClassToCxx(classInstance)
    classInstance.testMethod()
    passThroughSwiftClass(classInstance).testMethod()
}

testReceiveAndPassSwiftClass()

func testReceiveAndPassSwiftClassInClass() {
    let v = InClass(ExposedToCxx())
    v.getValue().i = 75
    v.getValue().testMethod()
}

testReceiveAndPassSwiftClassInClass()

#endif

// CHECK: ExposedToCxx.init
// CHECK-NEXT: ExposedToCxx0.testMethod
// CHECK-NEXT: ExposedToCxx12.testMethod
// CHECK-NEXT: ExposedToCxx-11.testMethod
// CHECK-NEXT: ExposedToCxx-11.testMethod
// CHECK-NEXT: ExposedToCxx2.testMethod
// CHECK-NEXT: ExposedToCxx2.deinit

// CHECK: ExposedToCxx.init
// CHECK-NEXT: ExposedToCxx75.testMethod
// CHECK-NEXT: destroy ~InClass()
// CHECK-NEXT: ExposedToCxx75.deinit

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:nonexistant")))
    __attribute__((swift_attr("release:nonexistant")))
NonExistant {};

struct
        __attribute__((swift_attr("import_reference")))
NoRetainRelease {};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain")))
    __attribute__((swift_attr("release:badRelease")))
BadRetainRelease {};

int badRetain(BadRetainRelease *v);
void badRelease(BadRetainRelease *v, int i);

//--- test.swift

import Test

// CHECK: error: cannot find retain function 'nonexistant' for reference type 'NonExistant'.
// CHECK: error: cannot find release function 'nonexistant' for reference type 'NonExistant'.
public func test(x: NonExistant) { }

// CHECK: error: reference type 'NoRetainRelease' must have 'retain:' swift attribute.
// CHECK: error: reference type 'NoRetainRelease' must have 'release:' swift attribute.
public func test(x: NoRetainRelease) { }

// CHECK: error: specified retain function 'badRetain' is invalid. Retain must have exactly one argument of type 'BadRetainRelease'
// CHECK: error: specified release function 'badRelease' is invalid. Release must have exactly one argument of type 'BadRetainRelease'
public func test(x: BadRetainRelease) { }
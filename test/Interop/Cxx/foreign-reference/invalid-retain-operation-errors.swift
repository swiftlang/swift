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
    __attribute__((swift_attr("retain:nonexistent")))
    __attribute__((swift_attr("release:nonexistent")))
NonExistent {};

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

// CHECK: error: cannot find retain function 'nonexistent' for reference type 'NonExistent'
// CHECK: error: cannot find release function 'nonexistent' for reference type 'NonExistent'
public func test(x: NonExistent) { }

// CHECK: error: reference type 'NoRetainRelease' must have 'retain:' swift attribute
// CHECK: error: reference type 'NoRetainRelease' must have 'release:' swift attribute
public func test(x: NoRetainRelease) { }

// CHECK: error: specified retain function 'badRetain' is invalid; retain function must have exactly one argument of type 'BadRetainRelease'
// CHECK: error: specified release function 'badRelease' is invalid; release function must have exactly one argument of type 'BadRetainRelease'
public func test(x: BadRetainRelease) { }

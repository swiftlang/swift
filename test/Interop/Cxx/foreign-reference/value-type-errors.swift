// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct __attribute__((swift_attr("import_reference"))) Ref {};

Ref returnsRef();
void takesRef(Ref r);

//--- test.swift

import Test

public func test(x: Ref) {
    // CHECK: note: function uses foreign reference type 'Ref' as a value in the return types which breaks 'swift_shared_reference' contract
    returnsRef()
    // CHECK: note: function uses foreign reference type 'Ref' as a value in a parameter types which breaks 'swift_shared_reference' contract
    takesRef(x)
}

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -cxx-interoperability-mode=default 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

//--- test.swift

import Test

// CHECK: error: do not pass both '-enable-experimental-cxx-interop' and '-cxx-interoperability-mode'; remove '-enable-experimental-cxx-interop'
// CHECK: note: Swift will maintain source compatibility for imported APIs based on the selected compatibility mode, so updating the Swift compiler will not change how APIs are imported

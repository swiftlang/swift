// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h


//--- test.swift

import Test

// CHECK: warning: the '-enable-experimental-cxx-interop' flag is deprecated; please pass '-cxx-interoperability-mode=' instead
// CHECK: note: Swift will maintain source compatibility for imported APIs based on the selected compatibility mode, so updating the Swift compiler will not change how APIs are imported

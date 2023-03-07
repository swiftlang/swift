// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -cxx-interoperability-mode=swift-5.8 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

//--- test.swift

import Test

// CHECK: error: invalid option passed to -cxx-interoperability-mode. Please select either 'off' or 'swift-5.9'.
// CHECK: note: Swift will maintain source compatibility for imported APIs based on the selected compatibility mode, so updating the Swift compiler will not change how APIs are imported.

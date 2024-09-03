// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -enable-experimental-feature NonescapableTypes -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"

struct SWIFT_NONESCAPABLE A {
    int a;
};

//--- test.swift

import Test

// CHECK: error: cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership
public func test() -> A {
    A()
}

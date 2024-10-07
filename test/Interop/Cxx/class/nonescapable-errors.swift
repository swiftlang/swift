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
#include <vector>

struct SWIFT_NONESCAPABLE View {
    View() : member(nullptr) {}
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

using VecOfPtr SWIFT_NONESCAPABLE = std::vector<int*>;

//--- test.swift

import CxxStdlib
import Test

// CHECK: error: cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership
public func noAnnotations() -> View {
    View()
}

// CHECK: error: cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership
public func noAnnotations2() -> VecOfPtr {
    VecOfPtr()
}

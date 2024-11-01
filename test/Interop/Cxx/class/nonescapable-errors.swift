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

struct SWIFT_NONESCAPABLE View {
    View() : member(nullptr) {}
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

struct SWIFT_ESCAPABLE Owner {
    int data;
};

Owner f(int* x [[clang::lifetimebound]]) { 
    return Owner{0};
}

Owner f2(int* x [[clang::lifetimebound]], int* y [[clang::lifetimebound]]) { 
    return Owner{0};
}

View g(int* x) {
    return View(x);
}

//--- test.swift

import Test

// CHECK: error: cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership
public func noAnnotations() -> View {
    // CHECK: nonescapable.h:15:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies
    f(nil)
    // CHECK: nonescapable.h:19:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies
    // No duplicate warning for f2:
    // CHECK-NOT: nonescapable.h:19
    f2(nil, nil)
    // CHECK: nonescapable.h:23:6: warning: the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated
    g(nil)
    return View()
}

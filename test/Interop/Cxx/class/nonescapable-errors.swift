// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -enable-experimental-feature NonescapableTypes -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 -verify -verify-ignore-unknown

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

template<typename T>
struct MyVector{};

using VecOfPtr SWIFT_NONESCAPABLE = MyVector<int*>;
using VecOfEscapablePtr = MyVector<int*>;

//--- test.swift

import Test

public func noAnnotations() -> View { // expected-error{{cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership}}
    View()
}

public func noAnnotations2() -> VecOfPtr { // expected-error{{cannot infer lifetime dependence, no parameters found that are either ~Escapable or Escapable with a borrowing ownership}}
    VecOfPtr()
}

public func noAnnotations3() -> VecOfEscapablePtr {
    VecOfEscapablePtr()
}

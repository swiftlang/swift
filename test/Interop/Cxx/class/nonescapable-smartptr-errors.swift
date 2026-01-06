// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %swift_src_root%{fs-sep}lib%{fs-sep}ClangImporter%{fs-sep}SwiftBridging -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -verify-ignore-unrelated \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}nonescapable.h \
// RUN:   -verify-additional-prefix LIFETIMES- \
// RUN:   -enable-experimental-feature LifetimeDependence

// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %swift_src_root%{fs-sep}lib%{fs-sep}ClangImporter%{fs-sep}SwiftBridging -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -verify-ignore-unrelated \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}nonescapable.h \
// RUN:   -verify-additional-prefix NO-LIFETIMES-

// REQUIRES: swift_feature_LifetimeDependence

// FIXME: On windows the following error is produced:
//     cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter,
//     specify '@_lifetime(borrow _Right)' or '@_lifetime(copy _Right)'
//     optional(_Ty2&& _Right) noexcept(is_nothrow_constructible_v<_Ty, _Ty2>) // strengthened
// UNSUPPORTED: OS=windows-msvc

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"
#include <memory>

struct SWIFT_NONESCAPABLE View {
    View() : member(nullptr) {}
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
std::unique_ptr<View> o1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
std::shared_ptr<View> o2();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}


//--- test.swift
import Test
import CxxStdlib

// expected-LIFETIMES-error@+3 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+2 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
// expected-NO-LIFETIMES-error@+1 {{a function cannot return a ~Escapable result}}
public func noAnnotations() -> View {
    o1()
    o2()
    return View()
}

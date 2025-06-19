// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s -check-prefix=CHECK-NO-LIFETIMES

// REQUIRES: swift_feature_LifetimeDependence

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

template<typename F, typename S>
struct SWIFT_ESCAPABLE_IF(F, S) MyPair {
    F first;
    S second;
};

MyPair<View, Owner> h1(int* x);
MyPair<Owner, View> h2(int* x);
MyPair<Owner, Owner> h3(int* x);

template<typename F, typename S>
struct SWIFT_ESCAPABLE_IF(F, Missing) MyPair2 {
    F first;
    S second;
};

template<typename F, int S>
struct SWIFT_ESCAPABLE_IF(F, S) MyType {
    F field;
};

MyPair2<Owner, Owner> i1();
MyType<Owner, 0> i2();

template<typename T>
struct Outer {
    struct NonTemplated {
        template <typename S>
        struct SWIFT_ESCAPABLE_IF(T, S) Inner {
            T t;
            S s;
        };
    };
};

Outer<View>::NonTemplated::Inner<Owner> j1();
Outer<Owner>::NonTemplated::Inner<View> j2();
Outer<Owner>::NonTemplated::Inner<Owner> j3();

template<typename... Ts>
struct SWIFT_ESCAPABLE_IF(Ts) MyTuple {};

MyTuple<View> k1();
MyTuple<Owner, View> k2();
MyTuple<Owner, Owner> k3();

using ViewVector = std::vector<View>;
using OwnerVector = std::vector<Owner>;

ViewVector l1();
OwnerVector l2();

const View* usedToCrash(const View* p) {
    return p;
}

struct SWIFT_ESCAPABLE Invalid {
    View v;
};

//--- test.swift
import Test
import CxxStdlib

// CHECK: error: cannot find type 'Invalid' in scope
// CHECK: note: escapable record 'Invalid' cannot have non-escapable field 'v'
public func importInvalid(_ x: Invalid) {
}

// CHECK: error: a function with a ~Escapable result needs a parameter to depend on
// CHECK-NO-LIFETIMES: test.swift:11:32: error: a function cannot return a ~Escapable result
public func noAnnotations() -> View {
    // CHECK: nonescapable.h:16:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies [#ClangDeclarationImport]
    // CHECK-NO-LIFETIMES: nonescapable.h:16:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies [#ClangDeclarationImport]
    f(nil)
    // CHECK: nonescapable.h:20:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies [#ClangDeclarationImport]
    // CHECK-NO-LIFETIMES: nonescapable.h:20:7: warning: the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies [#ClangDeclarationImport]
    // No duplicate warning for f2:
    // CHECK-NOT: nonescapable.h:20
    f2(nil, nil)
    // CHECK: nonescapable.h:24:6: warning: the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated [#ClangDeclarationImport]
    // CHECK-NO-LIFETIMES: nonescapable.h:24:6: warning: the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated [#ClangDeclarationImport]
    // CHECK-NO-LIFETIMES: nonescapable.h:24:6: error: a function cannot return a ~Escapable result
    g(nil)
    h1(nil)
    // CHECK-NO-LIFETIMES: nonescapable.h:34:21: error: a function cannot return a ~Escapable result
    h2(nil)
    // CHECK-NO-LIFETIMES: nonescapable.h:35:21: error: a function cannot return a ~Escapable result
    h3(nil)
    i1()
    // CHECK: nonescapable.h:39:39: error: template parameter 'Missing' does not exist
    // CHECK-NO-LIFETIMES: nonescapable.h:39:39: error: template parameter 'Missing' does not exist
    i2()
    // CHECK: nonescapable.h:45:33: error: template parameter 'S' expected to be a type parameter
    // CHECK-NO-LIFETIMES: nonescapable.h:45:33: error: template parameter 'S' expected to be a type parameter
    j1()
    // CHECK-NO-LIFETIMES: nonescapable.h:63:41: error: a function cannot return a ~Escapable result
    j2()
    // CHECK-NO-LIFETIMES: nonescapable.h:64:41: error: a function cannot return a ~Escapable result
    j3()
    k1();
    // CHECK-NO-LIFETIMES: nonescapable.h:70:15: error: a function cannot return a ~Escapable result
    k2();
    // CHECK-NO-LIFETIMES: nonescapable.h:71:22: error: a function cannot return a ~Escapable result
    k3();
    l1();
    // CHECK: nonescapable.h:77:12: error: a function with a ~Escapable result needs a parameter to depend on
    // CHECK-NO-LIFETIMES: nonescapable.h:77:12: error: a function cannot return a ~Escapable result
    l2();
    return View()
}

public func test3(_ x: inout View) {
    usedToCrash(&x)
    // CHECK: error: cannot find 'usedToCrash' in scope
    // CHECK: note: function 'usedToCrash' unavailable (cannot import)
    // CHECK: note: return type unavailable (cannot import)
    // CHECK: pointer to non-escapable type 'View' cannot be imported
    // CHECK-NO-LIFETIMES: error: cannot find 'usedToCrash' in scope
    // CHECK-NO-LIFETIMES: note: function 'usedToCrash' unavailable (cannot import)
    // CHECK-NO-LIFETIMES: note: return type unavailable (cannot import)
    // CHECK-NO-LIFETIMES: pointer to non-escapable type 'View' cannot be imported
}
    // CHECK-NOT: error
    // CHECK-NOT: warning
    // CHECK-NO-LIFETIMES-NOT: error
    // CHECK-NO-LIFETIMES-NOT: warning

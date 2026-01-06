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

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"
#include <vector>
#include <optional>

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

template<typename T>
struct SWIFT_ESCAPABLE TemplatedOwner {
    T data;
};

using TemplatedIntOwner = TemplatedOwner<int>;
using TemplatedFloatOwner = TemplatedOwner<float>;

// expected-warning@+1 {{the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies}}
Owner f(int* x [[clang::lifetimebound]]) { 
    return Owner{0};
}

// expected-warning@+1 {{the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies}}
Owner f2(int* x [[clang::lifetimebound]], int* y [[clang::lifetimebound]]) { 
    return Owner{0};
}

// expected-warning@+1 {{the returned type 'TemplatedIntOwner' is annotated as escapable; it cannot have lifetime dependencies}}
TemplatedIntOwner f3(int* x [[clang::lifetimebound]]) { 
    return TemplatedOwner<int>{0};
}

// expected-warning@+1 {{the returned type 'TemplatedFloatOwner' is annotated as escapable; it cannot have lifetime dependencies}}
TemplatedFloatOwner f4(int* x [[clang::lifetimebound]]) { 
    return TemplatedOwner<float>{0};
}

// expected-warning@+2 {{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
// expected-NO-LIFETIMES-error@+1 {{a function cannot return a ~Escapable result}}
View g(int* x) {
    return View(x);
}

template<typename F, typename S>
struct SWIFT_ESCAPABLE_IF(F, S) MyPair {
    F first;
    S second;
};

// expected-NO-LIFETIMES-error@+1 {{a function cannot return a ~Escapable result}}
MyPair<View, Owner> h1(int* x);

// expected-NO-LIFETIMES-error@+1 {{a function cannot return a ~Escapable result}}
MyPair<Owner, View> h2(int* x);

// OK; MyPair<Owner, Owner> is not ~Escapable
MyPair<Owner, Owner> h3(int* x);

// expected-error@+3 {{template parameter 'Missing' does not exist}}
// expected-error@+2 {{template parameter 'Missing' does not exist}}
template<typename F, typename S>
struct SWIFT_ESCAPABLE_IF(F, Missing) MyPair2 {
    F first;
    S second;
};

// expected-error@+2 {{template parameter 'S' expected to be a type parameter}}
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

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
Outer<View>::NonTemplated::Inner<Owner> j1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
Outer<Owner>::NonTemplated::Inner<View> j2();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

Outer<Owner>::NonTemplated::Inner<Owner> j3();

template<typename... Ts>
struct SWIFT_ESCAPABLE_IF(Ts) MyTuple {};

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
MyTuple<View> k1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
MyTuple<Owner, View> k2();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

MyTuple<Owner, Owner> k3();

using ViewVector = std::vector<View>;
using OwnerVector = std::vector<Owner>;

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
ViewVector l1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

OwnerVector l2();

// expected-note@+3 {{function 'usedToCrash' unavailable (cannot import)}}
// expected-note@+2 {{return type unavailable (cannot import)}}
// expected-note@+1 {{pointer to non-escapable type 'View' cannot be imported}}
const View* usedToCrash(const View* p) {
    return p;
}

// expected-note@+1 {{escapable record 'Invalid' cannot have non-escapable field 'v'}}
struct SWIFT_ESCAPABLE Invalid {
    View v;
};

// expected-note@+1 {{escapable record 'Invalid2' cannot have non-escapable base 'View'}}
struct SWIFT_ESCAPABLE Invalid2 : View {
};

struct SWIFT_NONESCAPABLE NonEscapable {};

using NonEscapableOptional = std::optional<NonEscapable>;

// Infered as non-escapable 
struct Aggregate {
  int a;
  View b;
  bool c;

  void someMethod() {}
}; 

// This is a complex record (has user-declared constructors), so we don't infer escapability.
// By default, it's imported as escapable, which generates an error 
// because of the non-escapable field 'View'
struct ComplexRecord {
  int a;
  View b;
  bool c;

  ComplexRecord() : a(1), b(), c(false) {}
  ComplexRecord(const ComplexRecord &other) = default;
}; 

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
Aggregate m1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

ComplexRecord m2(); // expected-note {{'m2()' has been explicitly marked unavailable here}}

// expected-error@+1 {{multiple SWIFT_NONESCAPABLE annotations found on 'DoubleNonEscapableAnnotation'}}
struct SWIFT_NONESCAPABLE SWIFT_NONESCAPABLE DoubleNonEscapableAnnotation {};
// expected-note@-1 {{SWIFT_NONESCAPABLE annotation found here}}
// expected-note@-2 {{SWIFT_NONESCAPABLE annotation found here}}

// expected-error@+1 {{multiple SWIFT_ESCAPABLE annotations found on 'DoubleEscapableAnnotation'}}
struct SWIFT_ESCAPABLE SWIFT_ESCAPABLE DoubleEscapableAnnotation {};
// expected-note@-1 {{SWIFT_ESCAPABLE annotation found here}}
// expected-note@-2 {{SWIFT_ESCAPABLE annotation found here}}

// expected-error@+3 {{multiple SWIFT_ESCAPABLE_IF annotations found on 'DoubleEscapableIfAnnotation<Owner, NonEscapable>'}}
// expected-error@+2 {{multiple SWIFT_ESCAPABLE_IF annotations found on 'DoubleEscapableIfAnnotation<Owner, DoubleEscapableAnnotation>'}}
template<typename F, typename S>
struct SWIFT_ESCAPABLE_IF(F) SWIFT_ESCAPABLE_IF(S) DoubleEscapableIfAnnotation {};
// expected-note@-1 {{SWIFT_ESCAPABLE_IF annotation found here}}
// expected-note@-2 {{SWIFT_ESCAPABLE_IF annotation found here}}
// expected-note@-3 {{SWIFT_ESCAPABLE_IF annotation found here}}
// expected-note@-4 {{SWIFT_ESCAPABLE_IF annotation found here}}

// expected-error@+1 {{multiple conflicting annotations found on 'EscapableNonEscapable'}}
struct SWIFT_ESCAPABLE SWIFT_NONESCAPABLE EscapableNonEscapable {};
// expected-note@-1 {{SWIFT_ESCAPABLE annotation found here}}
// expected-note@-2 {{SWIFT_NONESCAPABLE annotation found here}}

// expected-error@+1 {{multiple conflicting annotations found on 'DoubleEscapableNonEscapable'}}
struct SWIFT_ESCAPABLE SWIFT_NONESCAPABLE SWIFT_NONESCAPABLE SWIFT_ESCAPABLE DoubleEscapableNonEscapable {};
// expected-note@-1 {{SWIFT_ESCAPABLE annotation found here}}
// expected-note@-2 {{SWIFT_NONESCAPABLE annotation found here}}
// expected-note@-3 {{SWIFT_NONESCAPABLE annotation found here}}
// expected-note@-4 {{SWIFT_ESCAPABLE annotation found here}}

// expected-error@+2 {{multiple conflicting annotations found on 'EscapableIfEscapable<NonEscapable>'}}
template<typename T>
struct SWIFT_ESCAPABLE_IF(T) SWIFT_ESCAPABLE EscapableIfEscapable {};
// expected-note@-1 {{SWIFT_ESCAPABLE_IF annotation found here}}
// expected-note@-2 {{SWIFT_ESCAPABLE annotation found here}}

// expected-error@+2 {{multiple conflicting annotations found on 'NonEscapableIfEscapable<Owner>'}}
template<typename T>
struct SWIFT_ESCAPABLE_IF(T) SWIFT_NONESCAPABLE NonEscapableIfEscapable {};
// expected-note@-1 {{SWIFT_ESCAPABLE_IF annotation found here}}
// expected-note@-2 {{SWIFT_NONESCAPABLE annotation found here}}

// expected-error@+2 {{SWIFT_ESCAPABLE_IF is invalid because it is only supported in class templates}}
// expected-error@+1 {{multiple conflicting annotations found on 'NonTemplateEscapableIf'}}
struct SWIFT_ESCAPABLE SWIFT_ESCAPABLE_IF(T) NonTemplateEscapableIf {};
// expected-note@-1 {{SWIFT_ESCAPABLE annotation found here}}
// expected-note@-2 {{SWIFT_ESCAPABLE_IF annotation found here}}

// expected-warning@+3 {{the returned type 'DoubleNonEscapableAnnotation' is annotated as non-escapable; its lifetime dependencies must be annotated}}
// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
DoubleNonEscapableAnnotation n1();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

DoubleEscapableAnnotation n2();

DoubleEscapableIfAnnotation<Owner, NonEscapable> n3();

DoubleEscapableIfAnnotation<Owner, DoubleEscapableAnnotation> n4();

// expected-warning@+3 {{the returned type 'EscapableNonEscapable' is annotated as non-escapable; its lifetime dependencies must be annotated}}
// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
EscapableNonEscapable n5();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

// expected-warning@+3 {{the returned type 'DoubleEscapableNonEscapable' is annotated as non-escapable; its lifetime dependencies must be annotated}}
// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
DoubleEscapableNonEscapable n6();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

EscapableIfEscapable<NonEscapable> n7();

// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
NonEscapableIfEscapable<Owner> n8();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

NonTemplateEscapableIf n9();

// We infer that MyPair is ~Escapable from `NonEscapable`, but still emit diagnostics for `Missing`
// expected-LIFETIMES-error@+2 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+1 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
MyPair<NonEscapable, MyPair<Owner, MyPair2<NonEscapable, DoubleEscapableAnnotation>>> n10();
// expected-NO-LIFETIMES-error@-1 {{a function cannot return a ~Escapable result}}

// Don't emit the same diagnostic twice
MyPair<DoubleEscapableAnnotation, DoubleEscapableAnnotation> n11();


//--- test.swift
import Test
import CxxStdlib

// expected-error@+1 {{cannot find type 'Invalid' in scope}}
public func importInvalid(_ x: Invalid) {}

// expected-error@+1 {{cannot find type 'Invalid2' in scope}}
public func importInvalid(_ x: Invalid2) {}

// expected-LIFETIMES-error@+3 {{a function with a ~Escapable result needs a parameter to depend on}}
// expected-LIFETIMES-note@+2 {{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
// expected-NO-LIFETIMES-error@+1 {{a function cannot return a ~Escapable result}}
public func noAnnotations() -> View {
    f(nil)
    f2(nil, nil)
    f3(nil)
    f4(nil)
    g(nil)
    h1(nil)
    h2(nil)
    h3(nil)
    i1()
    i2()
    j1()
    j2()
    j3()
    k1()
    k2()
    k3()
    l1()
    l2()
    return View()
}

public func diagnoseInvalidSwiftAttributes() {
    n1()
    n2()
    n3()
    n4()
    n5()
    n6()
    n7()
    n8()
    n9()
    n10()
    n11()
}

public func test3(_ x: inout View) {
    usedToCrash(&x) // expected-error {{cannot find 'usedToCrash' in scope}}
}

public func optional() {
    _ = NonEscapableOptional()
}

public func inferedEscapability() {
    m1()
    m2() // expected-error {{'m2()' is unavailable: return type is unavailable in Swift}}
}

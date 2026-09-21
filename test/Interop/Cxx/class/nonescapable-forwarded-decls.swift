// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %t%{fs-sep}Inputs -emit-sil %t%{fs-sep}forwarded.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -I %t%{fs-sep}Inputs -emit-sil -verify -verify-ignore-unrelated %t%{fs-sep}escapes.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -diagnostic-style llvm

// REQUIRES: swift_feature_Lifetimes

//--- Inputs/module.modulemap
module Test {
    header "forwarding.h"
    requires cplusplus
}

//--- Inputs/forwarding.h
#include "swift/bridging"

struct SWIFT_NONESCAPABLE View {
    const int *p;
    View() : p(nullptr) {}
};

// A member inherited by a derived class is cloned, and the clone forwards to
// the base's member. Only the base carries the C++ annotations, so the clone
// has to be given the base's dependencies rather than inferring its own.
struct Base {
    int data;
    View borrowsSelf() const [[clang::lifetimebound]] { return View(); }
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View independent() const { return View(); }
    // Depends on the argument *and* on 'self'.
    View both(View v [[clang::lifetimebound]]) const [[clang::lifetimebound]] {
        return v;
    }
    // A lifetime annotation Swift cannot represent: imported as unsafe, with a
    // result that depends on nothing.
    View fromPointer(const int *p [[clang::lifetimebound]]) const {
        return View();
    }
};

struct Derived : public Base {};

// A property whose getter is itself synthesized, reached through inheritance:
// the clone's accessor forwards to the base's accessor.
struct PropertyBase {
    int data;
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View getThing() const SWIFT_COMPUTED_PROPERTY { return View(); }
};
struct PropertyDerived : public PropertyBase {};


// A virtual method on a reference type is reached through a synthesized thunk.
class Ref {
public:
    virtual View pick(View a [[clang::lifetimebound]], View b) const {
        return a;
    }
} SWIFT_SHARED_REFERENCE(retainRef, releaseRef);

inline void retainRef(Ref *) {}
inline void releaseRef(Ref *) {}

// Operators and getters become subscripts, properties and operator functions.
// Those forward to the imported member, so they carry its dependencies too.
struct Synthesized {
    int data;
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View operator[](int i) const { return View(); }
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View operator*() const { return View(); }
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View operator+(int i) const { return View(); }
    __attribute__((swift_attr("@_lifetime(immortal)")))
    View getProperty() const SWIFT_COMPUTED_PROPERTY { return View(); }
};

//--- forwarded.swift

import Test

// An inherited member has the dependencies of the member it forwards to.

@_lifetime(borrow d)
func inheritedBorrowsSelf(_ d: borrowing Derived) -> View {
  return d.borrowsSelf()
}

@_lifetime(immortal)
func inheritedIndependent(_ d: borrowing Derived) -> View {
  return d.independent()
}

@_lifetime(immortal)
func inheritedFromPointer(_ d: borrowing Derived, _ p: UnsafePointer<CInt>) -> View {
  return unsafe d.fromPointer(p)
}

// The result depends on the argument and on the object, so both have to be
// spelled out here.
@_lifetime(copy v, borrow d)
func inheritedBoth(_ d: borrowing Derived, _ v: View) -> View {
  return d.both(v)
}

// A thunked virtual method keeps the annotation on its parameters: the result
// depends on 'a' alone.
@available(SwiftStdlib 5.8, *)
@_lifetime(copy a)
func throughThunk(_ r: Ref, _ a: View, _ b: View) -> View {
  return r.pick(a, b)
}

// An inherited property's accessor forwards to the base's accessor, in either
// order of use.
@_lifetime(immortal)
func viaBaseProperty(_ b: PropertyBase) -> View { return b.thing }

@_lifetime(immortal)
func viaInheritedProperty(_ d: PropertyDerived) -> View { return d.thing }

// Synthesized subscripts, properties and operator functions.

@_lifetime(immortal)
func viaSubscript(_ s: Synthesized) -> View { return s[0] }

@_lifetime(immortal)
func viaPointee(_ s: Synthesized) -> View { return s.pointee }

@_lifetime(immortal)
func viaOperator(_ s: Synthesized) -> View { return s + 1 }

@_lifetime(immortal)
func viaProperty(_ s: Synthesized) -> View { return s.property }

// The forwarding functions the importer synthesizes carry the annotations of
// the members they forward to.
// CHECK-DAG: sil {{.*}}[clang Derived.__synthesizedBaseCall_borrowsSelf{{.*}}] {{.*}} : $@convention(cxx_method) (@in_guaranteed Derived) -> @lifetime(borrow address_for_deps 0) @owned View
// CHECK-DAG: sil {{.*}}[clang Derived.__synthesizedBaseCall_independent{{.*}}] {{.*}} : $@convention(cxx_method) (@in_guaranteed Derived) -> @lifetime(immortal) @owned View
// CHECK-DAG: sil {{.*}}[clang Derived.__synthesizedBaseCall_both{{.*}}] {{.*}} : $@convention(cxx_method) (View, @in_guaranteed Derived) -> @lifetime(copy 0, borrow address_for_deps 1) @owned View
// CHECK-DAG: sil {{.*}}[clang Ref.pick] {{.*}} : $@convention(cxx_method) (View, View, Ref) -> @lifetime(copy 0) @owned View

//--- escapes.swift

import Test

// 'both' depends on 'self' as well as on its argument, so the result cannot
// outlive the object, whether it is reached through the base or the derived
// class.
@_lifetime(copy v)
func inheritedBoth(_ v: View) -> View {
  let d = Derived()
  return d.both(v)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-3 {{it depends on the lifetime of variable 'd'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(copy v)
func baseBoth(_ v: View) -> View {
  let b = Base()
  return b.both(v)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-3 {{it depends on the lifetime of variable 'b'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

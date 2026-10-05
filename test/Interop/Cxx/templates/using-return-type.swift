// C++ methods that return iterators are imported as '@unsafe(always)', and their
// old '__{{METHOD_NAME}}Unsafe' spelling as a deprecated migration stub.
//
// In this test, we ensure that the iterator-detection logic does not depend on
// whether the iterator type happens to be instantiated at the time we determine
// the imported name of the C++ method.

// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}main.swift \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h \
// RUN:   -suppress-notes \
// RUN:   -cxx-interoperability-mode=default

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once

#include <iterator>

template <typename T> struct IteratorT {
  // Having this makes (each instance of) IteratorT considered an iterator
  using iterator_category = std::input_iterator_tag;
};

template <typename T> struct IdentityT {
  T t;
};

// Some types to instantiate IteratorT with
struct A {};
struct B {};
struct C {};
struct D {};

struct AA {
  IteratorT<A> getIter() const;
  IdentityT<A> getValue() const;
};

struct BB {
  using iter = IteratorT<B>;
  using value = IdentityT<B>;
  iter getIter() const;
  value getValue() const;
};

struct CC {
  using iter = IteratorT<C>;
  using value = IdentityT<C>;
  IteratorT<C> getIter() const;
  IdentityT<C> getValue() const;
};

struct DD {
  IteratorT<D> getIter() const;
  IdentityT<D> getValue() const;
  using iter = IteratorT<D>;
  using value = IdentityT<D>;
};

struct AAA : AA {};
struct BBB : BB {};
struct CCC : CC {};
struct DDD : DD {};

//--- main.swift
import CxxModule

let aa = AA()
aa.getIter() // expected-error {{must be marked with 'unsafe'}}
aa.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
aa.getValue()
aa.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let bb = BB()
bb.getIter() // expected-error {{must be marked with 'unsafe'}}
bb.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
bb.getValue()
bb.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let cc = CC()
cc.getIter() // expected-error {{must be marked with 'unsafe'}}
cc.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
cc.getValue()
cc.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let dd = DD()
dd.getIter() // expected-error {{must be marked with 'unsafe'}}
dd.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
dd.getValue()
dd.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let aaa = AAA()
aaa.getIter() // expected-error {{must be marked with 'unsafe'}}
aaa.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
aaa.getValue()
aaa.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let bbb = BBB()
bbb.getIter() // expected-error {{must be marked with 'unsafe'}}
bbb.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
bbb.getValue()
bbb.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let ccc = CCC()
ccc.getIter() // expected-error {{must be marked with 'unsafe'}}
ccc.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
ccc.getValue()
ccc.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let ddd = DDD()
ddd.getIter() // expected-error {{must be marked with 'unsafe'}}
ddd.__getIterUnsafe() // expected-warning {{'__getIterUnsafe()' is deprecated: renamed to 'getIter()'}}
ddd.getValue()
ddd.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

// C++ methods that return iterators are imported with an unsafe name mangling,
// i.e., '__{{METHOD_NAME}}Unsafe'.
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
aa.getIter() // expected-error {{has no member 'getIter'}}
aa.__getIterUnsafe()
aa.getValue()
aa.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let bb = BB()
bb.getIter() // expected-error {{has no member 'getIter'}}
bb.__getIterUnsafe()
bb.getValue()
bb.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let cc = CC()
cc.getIter() // expected-error {{has no member 'getIter'}}
cc.__getIterUnsafe()
cc.getValue()
cc.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let dd = DD()
dd.getIter() // expected-error {{has no member 'getIter'}}
dd.__getIterUnsafe()
dd.getValue()
dd.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let aaa = AAA()
aaa.getIter() // expected-error {{has no member 'getIter'}}
aaa.__getIterUnsafe()
aaa.getValue()
aaa.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let bbb = BBB()
bbb.getIter() // expected-error {{has no member 'getIter'}}
bbb.__getIterUnsafe()
bbb.getValue()
bbb.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let ccc = CCC()
ccc.getIter() // expected-error {{has no member 'getIter'}}
ccc.__getIterUnsafe()
ccc.getValue()
ccc.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

let ddd = DDD()
ddd.getIter() // expected-error {{has no member 'getIter'}}
ddd.__getIterUnsafe()
ddd.getValue()
ddd.__getValueUnsafe() // expected-error {{has no member '__getValueUnsafe'}}

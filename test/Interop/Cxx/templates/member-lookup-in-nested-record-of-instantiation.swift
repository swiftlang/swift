// Members of a record nested in an implicit class template instantiation.
//
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}storage-first.swift \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}lookup-first.swift \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -suppress-notes

//--- Inputs/module.modulemap
module CxxModule {
  requires cplusplus
  header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once

template <class T>
struct Outer {
  struct Inner {
    T x;
    T get() const { return x; }
  };
  Inner makeInner() const { return {42}; }
};

using OuterInt = Outer<int>;

//--- storage-first.swift
import CxxModule

func requireSendable<T: Sendable>(_ t: T) {}

func sendableThenMethod(_ o: OuterInt) -> CInt {
  let i = o.makeInner()
  requireSendable(i) // loads Inner's stored properties
  return i.get() + i.x
}

//--- lookup-first.swift
import CxxModule

func requireSendable<T: Sendable>(_ t: T) {}

func methodThenSendable(_ o: OuterInt) -> CInt {
  let i = o.makeInner()
  let r = i.get() // expected-error {{value of type 'Outer<CInt>.Inner' has no member 'get'}}
                  // FIXME: this lookup should succeed, as it does in storage-first.swift.
  requireSendable(i)
  return r
}

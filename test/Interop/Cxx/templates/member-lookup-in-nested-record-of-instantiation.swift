// Members of a record nested in an implicit class template instantiation.
// Lookup must not depend on whether the record's storage was loaded first.
//
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}storage-first.swift \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}lookup-first.swift \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default

//--- Inputs/module.modulemap
module CxxModule {
  requires cplusplus
  header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once

template <class T>
struct Outer {
  enum { OuterEnumA = 3 };

  struct Inner {
    T x;
    T get() const { return x; }

    enum { InnerEnumA = 1, InnerEnumB = 2 };
    enum Color { Red, Green };

    struct Deeper {
      T y;
      T getY() const { return y; }
      enum { DeeperEnumA = 4 };
    };
    Deeper makeDeeper() const { return {x}; }
  };
  Inner makeInner() const { return {42}; }

  struct Base {
    T b;
    T getB() const { return b; }
  };
  struct Derived : Base {
    T d;
  };
  Derived makeDerived() const { return {}; }
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

func sendableThenEnumConstants(_ o: OuterInt) {
  let i = o.makeInner()
  requireSendable(i) // loads Inner's stored properties
  _ = type(of: i).InnerEnumA
  _ = type(of: i).InnerEnumB
  _ = type(of: i).Red
  _ = type(of: i).Green
}

//--- lookup-first.swift
import CxxModule

func requireSendable<T: Sendable>(_ t: T) {}

func methodThenSendable(_ o: OuterInt) -> CInt {
  let i = o.makeInner()
  let r = i.get()
  requireSendable(i)
  return r
}

func enumConstantsThenSendable(_ o: OuterInt) {
  let i = o.makeInner()
  _ = type(of: i).InnerEnumA
  _ = type(of: i).InnerEnumB
  _ = type(of: i).Red
  _ = type(of: i).Green
  requireSendable(i)
}

func outerEnumConstant() {
  _ = OuterInt.OuterEnumA
}

func deeperMembers(_ o: OuterInt) -> CInt {
  let d = o.makeInner().makeDeeper()
  _ = type(of: d).DeeperEnumA
  return d.getY() + d.y
}

func inheritedMembers(_ o: OuterInt) -> CInt {
  let d = o.makeDerived()
  return d.getB() + d.b + d.d
}

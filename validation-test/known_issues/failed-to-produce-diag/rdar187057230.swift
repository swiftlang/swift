// rdar://187057230
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/A.swift -module-name A -o %t/A.swiftmodule -I %t/Inputs -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck -verify %t/B.swift -I %t/Inputs -I %t -cxx-interoperability-mode=default

//--- Inputs/module.modulemap
module CxxMod {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
#pragma once

struct MapProxy {
private:
  template <class T>
  struct Iterator {
    Iterator &operator++() { ++pos; return *this; }
    const int *pos;
  };

public:
  using const_iterator = Iterator<int>;
  const_iterator begin() const { return {nullptr}; }
  const_iterator end() const { return {nullptr}; }
};

//--- A.swift
import CxxMod

extension MapProxy: CxxSequence {
  public typealias RawIterator = const_iterator
}

extension MapProxy.const_iterator: UnsafeCxxInputIterator {
  public static func ==(lhs: Self, rhs: Self) -> Bool { lhs.pos == rhs.pos }
  public var pointee: Int32 { pos.pointee }
  public func __operatorStar() -> UnsafePointer<Int32> { pos }
}

//--- B.swift
import CxxMod
import A

func f(_ list: MapProxy) {
  for x in list { _ = x } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

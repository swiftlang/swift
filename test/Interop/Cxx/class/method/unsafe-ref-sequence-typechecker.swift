// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -I %t/Inputs %t/test.swift \
// RUN:   -cxx-interoperability-mode=default

// XFAIL: OS=linux-androideabi

//--- Inputs/module.modulemap
module UnsafeRefContainer {
  header "unsafe-ref-container.h"
  requires cplusplus
}

//--- Inputs/unsafe-ref-container.h
#pragma once
#include <vector>

struct __attribute__((swift_attr("import_reference")))
       __attribute__((swift_attr("retain:immortal")))
       __attribute__((swift_attr("release:immortal")))
       __attribute__((swift_attr("unsafe")))
UnsafeRef {
  int doSomething(int x) { return x; }
};

class Container {
  UnsafeRef *storage;
  int count;
public:
  const UnsafeRef *begin() const { return storage; }
  const UnsafeRef *end() const { return storage + count; }
};

struct CWrapper {
  Container items;
};

struct VWrapper {
  std::vector<UnsafeRef> items;
};

//--- test.swift

import UnsafeRefContainer
import CxxStdlib

func use() {
  let cw = CWrapper()
  _ = cw.items.__beginUnsafe()
  _ = cw.items.__endUnsafe()
  _ = cw.items.begin() // expected-error {{value of type 'Container' has no member 'begin'}}
  // expected-note@-1 {{C++ method 'begin' that returns an iterator is unavailable}}
  // expected-note@-2 {{C++ methods that return iterators are potentially unsafe; try using Swift collection APIs instead}}
  _ = cw.items.end() // expected-error {{value of type 'Container' has no member 'end'}}
  // expected-note@-1 {{C++ method 'end' that returns an iterator is unavailable}}
  // expected-note@-2 {{C++ methods that return iterators are potentially unsafe; try using Swift collection APIs instead}}

  let vw = VWrapper()
  _ = vw.items.__beginUnsafe()
  _ = vw.items.__endUnsafe()
  _ = vw.items.begin() // expected-error {{has no member 'begin'}}
  // expected-note@-1 {{C++ method 'begin' that returns an iterator is unavailable}}
  // expected-note@-2 {{do you want to use a for-in loop instead?}}
  _ = vw.items.end() // expected-error {{has no member 'end'}}
  // expected-note@-1 {{C++ method 'end' that returns an iterator is unavailable}}
  // expected-note@-2 {{do you want to compare against 'nil' instead?}}
  for i in vw.items {}
}

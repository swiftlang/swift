// RUN: rm -rf %t
// RUN: split-file %s %t

// From C++20 on, a type that declares only 'iterator_concept' is an iterator
// ([iterator.traits]), so a method returning one does not keep the storage it
// points into alive and is imported under the unsafe spelling.
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}cxx20.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -strict-memory-safety \
// RUN:   -Xcc -std=c++20 \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}iter.h

// Before C++20 the typedef has no meaning, and the type is an ordinary value.
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}cxx17.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -strict-memory-safety \
// RUN:   -Xcc -std=c++17

//--- Inputs/module.modulemap
module IterConcept {
    header "iter.h"
    requires cplusplus
}

//--- Inputs/iter.h
struct Iter {
  using iterator_concept = int;
  int operator*() const;
};

struct HasIter {
  int x;
  // expected-note@+1 {{this returns an iterator, which does not keep the underlying storage alive}}
  Iter get() const;
};

//--- cxx20.swift
import IterConcept

func useIterator(_ h: HasIter) {
  _ = h.__getUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getUnsafe()'}}
}

//--- cxx17.swift
import IterConcept

func useValue(_ h: HasIter) {
  _ = h.get()
}

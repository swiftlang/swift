// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}iter.h

// An iterator may get its 'iterator_category' from an explicit specialization
// of std::iterator_traits rather than from a member. A method returning one
// does not keep the storage it points into alive and is imported under the
// unsafe spelling.

//--- Inputs/module.modulemap
module TraitsIter {
    header "iter.h"
    requires cplusplus
}

//--- Inputs/iter.h
struct Iter {
  int operator*() const;
};

namespace std {
template <class T> struct iterator_traits;
template <> struct iterator_traits<Iter> {
  using iterator_category = int;
};
} // namespace std

struct HasIter {
  int x;
  // expected-note@+1 {{this returns an iterator, which does not keep the underlying storage alive}}
  Iter get() const;
};

//--- test.swift
import TraitsIter

func useIterator(_ h: HasIter) {
  _ = h.__getUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getUnsafe()'}}
}

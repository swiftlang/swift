// RUN: rm -rf %t
// RUN: split-file %s %t

// An iterator may inherit 'iterator_category' or, from C++20 on,
// 'iterator_concept' from a base class. A method returning one does not keep
// the storage it points into alive and is imported under the unsafe spelling.
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}cxx20.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -Xcc -std=c++20 \
// RUN:   -verify-additional-prefix cxx20- \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}iter.h

// Before C++20 only the inherited 'iterator_category' counts.
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}cxx17.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -Xcc -std=c++17 \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}iter.h

//--- Inputs/module.modulemap
module InheritedTag {
    header "iter.h"
    requires cplusplus
}

//--- Inputs/iter.h
struct BaseWithCategory {
  using iterator_category = int;
};
struct InheritsCategory : BaseWithCategory {
  int operator*() const;
};

struct BaseWithConcept {
  using iterator_concept = int;
};
struct InheritsConcept : BaseWithConcept {
  int operator*() const;
};

struct HasInheritedIter {
  int x;
  // expected-note@+1 {{this returns an iterator, which does not keep the underlying storage alive}}
  InheritsCategory getCategory() const;
  // expected-cxx20-note@+1 {{this returns an iterator, which does not keep the underlying storage alive}}
  InheritsConcept getConcept() const;
};

//--- cxx20.swift
import InheritedTag

func useInherited(_ h: HasInheritedIter) {
  _ = h.__getCategoryUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getCategoryUnsafe()'}}
  _ = h.__getConceptUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getConceptUnsafe()'}}
}

//--- cxx17.swift
import InheritedTag

func useInherited(_ h: HasInheritedIter) {
  _ = h.__getCategoryUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getCategoryUnsafe()'}}
  _ = h.getConcept()
}

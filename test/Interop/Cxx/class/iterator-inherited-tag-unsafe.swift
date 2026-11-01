// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -strict-memory-safety \
// RUN:   -Xcc -std=c++20

// An iterator whose 'iterator_category' or 'iterator_concept' comes from a
// base class is not recognized as an iterator yet, so a method returning one
// is imported as an ordinary safe method.

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
  InheritsCategory getCategory() const;
  InheritsConcept getConcept() const;
};

//--- test.swift
import InheritedTag

// FIXME: both should be '__getCategoryUnsafe' and '__getConceptUnsafe'.
func useInherited(_ h: HasInheritedIter) {
  _ = h.getCategory()
  _ = h.getConcept()
}

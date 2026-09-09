// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}proj.h

// Using an unsafe C++ projection should say why it is unsafe, at the C++
// declaration responsible.

//--- Inputs/module.modulemap
module Proj {
    header "proj.h"
    requires cplusplus
}

//--- Inputs/proj.h
#include "swift/bridging"

// The user-declared copy constructor makes Owner self-contained, so returning
// a pointer out of it is a projection.
// expected-note@+1 2 {{type 'Owner' has unknown escapability because Swift cannot infer it from the type's members; annotate the type with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct Owner {
  void *ptr;
  Owner(const Owner &);

  // expected-note@+1 2 {{'data' is unsafe because it returns a pointer or reference into a type that owns its storage}}
  int *data() const;
};

// A method returning an iterator does not keep the storage it points into
// alive, whoever owns it.
struct Iter {
  using iterator_category = int;
  int operator*() const;
};
struct HasIter {
  int x;
  // expected-note@+1 {{'get' is unsafe because it returns an iterator, which does not keep the underlying storage alive}}
  Iter get() const;
};

// A view projected out of a self-contained type can outlive what it views.
struct SWIFT_NONESCAPABLE Slice {
  const int *p;
};
// expected-note@+1 {{type 'SelfContained' has unknown escapability because Swift cannot infer it from the type's members; annotate the type with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct SelfContained {
  void *ptr;
  SelfContained(const SelfContained &);

  // expected-note@+1 {{'slice' is unsafe because it returns a view into a type that owns its storage}}
  Slice slice() const [[clang::lifetimebound]];
};

// A handful of standard library methods are known to be hard to use correctly.
// Declared here rather than included, since a note in a real system header is
// suppressed -- see no-reason-in-system-header.swift.
namespace std {
template <class T>
struct set {
  T x;
  // expected-note@+1 {{'insert' is unsafe because this standard library method is known to be hard to use correctly from Swift}}
  void insert(T value);
};
} // namespace std
using IntSet = std::set<int>;

// A member inherited into a derived type is imported as a clone of the base's,
// and is explained by the rule applied to the original.
// expected-note@+1 {{type 'Derived' has unknown escapability because its escapability depends on 'Owner', whose escapability is unknown}}
struct Derived : Owner {};

//--- test.swift
import Proj

func use(_ o: Owner) {
  _ = unsafe o.__dataUnsafe()
  _ = o.__dataUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__dataUnsafe()'}}
  // expected-note@-2 {{reference to parameter 'o' involves unsafe type 'Owner'}}
}

func useIterator(_ h: HasIter) {
  _ = h.__getUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__getUnsafe()'}}
}

func useView(_ s: SelfContained) {
  _ = s.__sliceUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__sliceUnsafe()'}}
  // expected-note@-2 {{reference to parameter 's' involves unsafe type 'SelfContained'}}
}

func useStdMethod(_ s: IntSet) {
  var set = s
  set.__insertUnsafe(1) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__insertUnsafe'}}
}

// The note lands on the base's method, which is the one the rule was applied to.
func useClonedMember(_ d: Derived) {
  _ = d.__dataUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__dataUnsafe()'}}
  // expected-note@-2 {{reference to parameter 'd' involves unsafe type 'Derived'}}
}

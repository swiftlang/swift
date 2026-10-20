// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift

// A function template renamed with swift_name can be found by its Swift name.

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct HasRenamedMemberTemplates {
  int value;

  template <class T>
  __attribute__((swift_name("renamedGeneric(_:)")))
  int genericMember(T v) const { return value + static_cast<int>(v); }

  // The template parameter is unused, so this is imported as non-generic.
  template <class T = int>
  __attribute__((swift_name("renamedDefaulted(_:)")))
  int defaultedMember(int v) const { return value + v; }

  template <class T>
  __attribute__((swift_name("renamedStatic(from:)")))
  static HasRenamedMemberTemplates staticMember(T v) {
    return HasRenamedMemberTemplates{static_cast<int>(v)};
  }
};

struct HasLabeledMemberTemplates {
  int value;

  template <class T, class U>
  __attribute__((swift_name("labeled(first:second:)")))
  int twoLabels(T a, U b) const {
    return value + static_cast<int>(a) + static_cast<int>(b);
  }

  template <class T>
  __attribute__((swift_name("mixed(_:label:)")))
  int unlabeledThenLabeled(T a, int b) const {
    return value + static_cast<int>(a) + b;
  }

  // Same Swift base name, distinguished only by argument labels.
  template <class T>
  __attribute__((swift_name("pick(byA:)")))
  int pickA(T v) const { return 10 + static_cast<int>(v); }

  template <class T>
  __attribute__((swift_name("pick(byB:)")))
  int pickB(T v) const { return 20 + static_cast<int>(v); }

  // Renamed to the base name of an existing non-template member.
  int existing(int v) const { return v; }

  template <class T>
  __attribute__((swift_name("existing(withLabel:)")))
  int renamedToExisting(T v) const { return static_cast<int>(v); }

  template <class T>
  __attribute__((swift_name("makeLabeled(from:and:)")))
  static HasLabeledMemberTemplates staticTwoLabels(T a, int b) {
    return HasLabeledMemberTemplates{static_cast<int>(a) + b};
  }
};

namespace NS {
template <class T>
__attribute__((swift_name("renamedInNamespace(_:)")))
T namespaceTemplate(T v) { return v; }

template <class T>
__attribute__((swift_name("labeledInNamespace(value:)")))
T labeledNamespaceTemplate(T v) { return v; }
} // namespace NS

template <class T>
__attribute__((swift_name("renamedTopLevel(_:)")))
T topLevelTemplate(T v) { return v; }

template <class T, class U>
__attribute__((swift_name("labeledTopLevel(_:other:)")))
T labeledTopLevelTemplate(T v, U w) { return v + static_cast<T>(w); }

//--- test.swift
import Test

let s = HasRenamedMemberTemplates(value: 1)
let _: CInt = s.renamedGeneric(2 as CInt)
let _: CInt = s.renamedDefaulted(3)

let made = HasRenamedMemberTemplates.renamedStatic(from: 4 as CInt)
let _: CInt = made.value

let _: CInt = NS.renamedInNamespace(5 as CInt)
let _: CInt = renamedTopLevel(6 as CInt)

let l = HasLabeledMemberTemplates(value: 1)
let _: CInt = l.labeled(first: 2 as CInt, second: 3 as CInt)
let _: CInt = l.mixed(4 as CInt, label: 5)
let _: CInt = l.pick(byA: 6 as CInt)
let _: CInt = l.pick(byB: 7 as CInt)
let _: CInt = l.existing(8)
let _: CInt = l.existing(withLabel: 9 as CInt)

let madeLabeled = HasLabeledMemberTemplates.makeLabeled(from: 10 as CInt, and: 11)
let _: CInt = madeLabeled.value

let _: CInt = NS.labeledInNamespace(value: 12 as CInt)
let _: CInt = labeledTopLevel(13 as CInt, other: 14 as CInt)

// The C++ argument labels (none) must not be accepted.
let _ = l.labeled(2 as CInt, 3 as CInt) // expected-error {{missing argument labels 'first:second:' in call}}

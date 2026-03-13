// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Validate usability of std-contain-incomplete.h in C++ with clang
// RUN: %target-clangxx -c -o /dev/null -Xclang -verify -std=c++17 -I %t%{fs-sep}Inputs %t%{fs-sep}ok.cpp
//
// It isn't particularly helpful to -verify diagnostics when most of them come
// from the C++ stdlib; to keep things simple, we only use `not` to verify that
// err.cpp indeed causes compilation errors:
// RUN: not %target-clangxx -c -o /dev/null -Xclang -verify -std=c++17 -I %t%{fs-sep}Inputs %t%{fs-sep}err.cpp
//
// Note that above, we specify c++17 in order to use and test std::optional.
//
// Compare usability of std-contain-incomplete.h in Swift with swift-frontend
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}std-contain-incomplete.h \
// RUN:   %t%{fs-sep}ok.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -verify-ignore-unrelated \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}std-contain-incomplete.h \
// RUN:   %t%{fs-sep}err.swift -verify-additional-prefix swift-
//
// Check module interface of std-contain-incomplete.h
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -cxx-interoperability-mode=default -I %t/Inputs \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -module-to-print=StdContain | %FileCheck %s
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

//--- Inputs/module.modulemap
module StdContain {
  header "std-contain-incomplete.h"
  requires cplusplus
}

//--- Inputs/std-contain-incomplete.h
#pragma once
#include <optional>
#include <map>
#include <vector>

struct Incomplete;

// None of GoodStruct's members should prevent it from being imported sans error
struct GoodStruct {

  // For regular records like this, we can have declarations of function members
  // whose signature involves incomplete types, as long as we don't define them
  // (because checking those definitions would require those complete types).
  // This is in contrast to class template (below) where we can even provide
  // a definition for function members whose signature is dependent.

  std::optional<Incomplete> returnOptional() const; // expected-swift-note {{explicitly marked unavailable here}}
  std::map<Incomplete, int> returnMapKey() const;   // expected-swift-note {{explicitly marked unavailable here}}
  std::map<int, Incomplete> returnMapValue() const; // expected-swift-note {{explicitly marked unavailable here}}
  std::vector<Incomplete> returnVector() const;     // expected-swift-note {{explicitly marked unavailable here}}

  void takesOptional(std::optional<Incomplete> a) const; // expected-swift-note {{unavailable (cannot import)}}
                                                         // expected-swift-note@-1 {{parameter 'a' unavailable (cannot import)}}
  void takesMapKey(std::map<Incomplete, int> a) const;   // expected-swift-note {{unavailable (cannot import)}}
                                                         // expected-swift-note@-1 {{parameter 'a' unavailable (cannot import)}}
  void takesMapValue(std::map<int, Incomplete> a) const; // expected-swift-note {{unavailable (cannot import)}}
                                                         // expected-swift-note@-1 {{parameter 'a' unavailable (cannot import)}}
  void takesVector(std::vector<Incomplete> a) const;     // expected-swift-note {{unavailable (cannot import)}}
                                                         // expected-swift-note@-1 {{parameter 'a' unavailable (cannot import)}}

  using usingOptional = std::optional<Incomplete>;
  using usingMapKey = std::map<Incomplete, int>;
  using usingMapValue = std::map<int, Incomplete>;
  using usingVector = std::vector<Incomplete>;
};
// CHECK:      struct GoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func returnOptional() -> Never
// CHECK-NEXT:   func returnMapKey() -> Never
// CHECK-NEXT:   func returnMapValue() -> Never
// CHECK-NEXT:   func returnVector() -> Never
// CHECK-NEXT: }

template <typename T> // (we will later instantiate T to be Incomplete)
struct Template {

  // For templates, we can even provide definitions for function members whose
  // type signature involves incomplete types, as long as those incomplete types
  // are dependent (on the template type parameter). Even once the template is
  // instantiated with an incomplete type parameter, the template is usable as
  // long as one does not try to touch those function members.
  //
  // That said, we don't provide a definition for the functions involving
  // std::optional<Incomplete> because that produces heinous error messages that
  // are not helpful to -verify against. The reason for this difference is
  // probably due to the fact that std::optional<T> holds T by value.

  std::optional<T> returnOptional() const;               // expected-swift-note {{explicitly marked unavailable here}}
  std::map<T, int> returnMapKey() const { return {}; }   // expected-swift-note {{explicitly marked unavailable here}}
  std::map<int, T> returnMapValue() const { return {}; } // expected-swift-note {{explicitly marked unavailable here}}
  std::vector<T> returnVector() const { return {}; }     // expected-swift-note {{explicitly marked unavailable here}}

  void takesOptional(std::optional<T> a) const;
  void takesMapKey(std::map<T, int> a) const {}
  void takesMapValue(std::map<int, T> a) const {}
  void takesVector(std::vector<T> a) const {}

  using usingOptional = std::optional<T>;
  using usingVector = std::vector<T>;
  using usingMapKey = std::map<T, int>;
  using usingMapValue = std::map<int, T>;
};

using UsableTemplate = Template<Incomplete>;

// CHECK:      struct Template<Incomplete> {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func returnOptional() -> Never
// CHECK-NEXT:   func returnMapKey() -> Never
// CHECK-NEXT:   func returnMapValue() -> Never
// CHECK-NEXT:   func returnVector() -> Never
// CHECK-NEXT: }

//--- ok.cpp
// expected-no-diagnostics
#include <std-contain-incomplete.h>

void ok(void) {
  GoodStruct gs;
  UsableTemplate ut;
}

//--- err.cpp
#include <std-contain-incomplete.h>

void err(void) {
  // There's not a good way to validate these errors in a regression test,
  // since most of the errors are tied to source locations from the C++ stdlib.

  GoodStruct gs;
  auto o = gs.returnOptional();
  auto v = gs.returnVector();
  auto k = gs.returnMapKey();
  auto x = gs.returnMapValue();

  gs.takesOptional(o);
  gs.takesVector(v);
  gs.takesMapKey(k);
  gs.takesMapValue(x);
}

void errTmpl(void) {
  UsableTemplate ut;
  auto o = ut.returnOptional();
  auto v = ut.returnVector();
  auto k = ut.returnMapKey();
  auto x = ut.returnMapValue();

  ut.takesOptional(o);
  ut.takesVector(v);
  ut.takesMapKey(k);
  ut.takesMapValue(x);
}

//--- ok.swift
import StdContain

func ok() {
  let _ = GoodStruct()
  let _ = UsableTemplate()
}

//--- err.swift
import StdContain

func err() {
  let gs = GoodStruct()
  let o = gs.returnOptional() // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let k = gs.returnMapKey()   // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let x = gs.returnMapValue() // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let v = gs.returnVector()   // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}

  gs.takesOptional(o) // expected-swift-error {{has no member}}
  gs.takesMapKey(k)   // expected-swift-error {{has no member}}
  gs.takesMapValue(x) // expected-swift-error {{has no member}}
  gs.takesVector(v)   // expected-swift-error {{has no member}}

  let _: usingOptional // expected-swift-error {{cannot find type}}
  let _: usingVector   // expected-swift-error {{cannot find type}}
  let _: usingMapKey   // expected-swift-error {{cannot find type}}
  let _: usingMapValue // expected-swift-error {{cannot find type}}
}

func errTemplates() {
  let ut = UsableTemplate()
  let o = ut.returnOptional() // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let v = ut.returnVector()   // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let k = ut.returnMapKey()   // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}
  let x = ut.returnMapValue() // expected-swift-error {{unavailable}}
                              // expected-swift-warning@-1 {{inferred to have type 'Never'}}
                              // expected-swift-note@-2 {{add an explicit type annotation to silence this warning}}

  ut.takesOptional(o) // expected-swift-error {{has no member}}
  ut.takesVector(v)   // expected-swift-error {{has no member}}
  ut.takesMapKey(k)   // expected-swift-error {{has no member}}
  ut.takesMapValue(x) // expected-swift-error {{has no member}}

  let _: usingOptional // expected-swift-error {{cannot find type}}
  let _: usingVector   // expected-swift-error {{cannot find type}}
  let _: usingMapKey   // expected-swift-error {{cannot find type}}
  let _: usingMapValue // expected-swift-error {{cannot find type}}
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Validate usability of function.h in C++ with clang
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/ok.cpp
// RUN: %target-clang -c -o /dev/null -Xclang -verify=cxx -I %t/Inputs %t/err.cpp
//
// Compare usability of function.h in Swift with swift-frontend
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}function.h \
// RUN:   %t%{fs-sep}ok.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}function.h \
// RUN:   %t%{fs-sep}err.swift -verify-additional-prefix swift-
//
// Check module interface of function.h
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -cxx-interoperability-mode=default -I %t/Inputs \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -module-to-print=Function | %FileCheck %s
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

//--- Inputs/module.modulemap
module Function {
  header "function.h"
  requires cplusplus
}

//--- Inputs/function.h
#pragma once

// NOTE: Bro<Ken> is an invalid type and will produce an error when instantiated

template <typename K>
struct Bro {
  typename K::enough iAm; // cxx-error {{no type named 'enough'}}
                          // expected-swift-error@-1 {{no type named 'enough'}}
};

struct Ken {};

// None of GoodStruct's members should prevent it from being imported sans error
struct GoodStruct {
  GoodStruct() = default;

  // FIXME: constructors are imported eagerly, so having this member would make GoodStruct unusable
  // GoodStruct(Bro<Ken>, Bro<Ken>);
  // NOTE: if the above has only one argument, Bro<Ken> will get instantiated
  // while considering the copy constructor overload candidate.

  // expected-swift-note@+3 {{explicitly marked unavailable here}}
  // expected-swift-note@+2 {{explicitly marked unavailable here}}
  // expected-swift-note@+1 {{explicitly marked unavailable here}}
  Bro<Ken> badReturn() const;
  // expected-swift-note@-1 {{requested here}}

  // expected-swift-note@+2 {{unavailable (cannot import)}}
  // expected-swift-note@+1 {{unavailable (cannot import)}}
  void badArg(Bro<Ken>) const;

  // expected-swift-note@+2 {{unavailable (cannot import)}}
  // expected-swift-note@+1 {{unavailable (cannot import)}}
  static Bro<Ken> badStatic(Bro<Ken>);

  void overloadsSameNumArgs(int) const;
  void overloadsSameNumArgs(Bro<Ken>) const;

  // expected-swift-note@+1 {{declared here}}
  void overloadsDiffNumArgs(int, int) const;
  void overloadsDiffNumArgs(Bro<Ken>) const;

  // FIXME: operators are imported eagerly, so having this member would make GoodStruct unusable
  // int operator+(Bro<Ken>) const;

  // FIXME: begin()/end() are imported eagerly, so having this member would make GoodStruct unusable
  // Bro<Ken> begin() const;
  // Bro<Ken> end() const;
};
// CHECK:      struct GoodStruct {
// CHECK-NEXT:   init()
//
// CHECK-NEXT:   func badReturn() -> Never
//
// NOTE-MISSING: func badArg(_: Never)
//
// NOTE-MISSING: func badStatic(_: Never) -> Never
//
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// NOTE-MISSING: func overloadsSameNumArgs(_: Never)
//
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// NOTE-MISSING: func overloadsDiffNumArgs(_: Never)
//
// CHECK-NEXT: }


struct DerivedGoodStruct : GoodStruct {};
// CHECK:      struct DerivedGoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func badReturn() -> Never
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// CHECK-NEXT: }

struct UsingGoodStruct : GoodStruct {
  using GoodStruct::badReturn;
  using GoodStruct::badArg;
  using GoodStruct::badStatic;
};

// CHECK:      struct UsingGoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func badReturn() -> Never
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// CHECK-NEXT: }


struct UsesGoodStruct {
  GoodStruct field;
  GoodStruct returns() const;
  void takes(GoodStruct) const;
};
// CHECK:      struct UsesGoodStruct {
// CHECK-NEXT:   init(field: GoodStruct)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var field: GoodStruct
// CHECK-NEXT:   func returns() -> GoodStruct
// CHECK-NEXT:   func takes(_: GoodStruct)
// CHECK-NEXT: }

//--- ok.cpp
// expected-no-diagnostics
#include <function.h>

void ok(void) {
  GoodStruct gs;
  gs.overloadsDiffNumArgs(42, 24);
  DerivedGoodStruct dgs;
  UsingGoodStruct ugs;

  UsesGoodStruct usgs;
  auto f = usgs.field;
  f = usgs.returns();
  usgs.takes(f);
}

//--- err.cpp
#include <function.h>

void err(void) {
  GoodStruct gs;
  gs.overloadsSameNumArgs(42); // cxx-note {{requested here}}
  // The Bro<Ken> type is instantiated while considering overloads candidates
  // for GoodStruct::overloadsSameNumArgs(_).

  // It is only instantiated once and thus no longer triggers diagnostics in
  // its following uses:
  auto inc = gs.badReturn();
  gs.badArg(inc);
  GoodStruct::badStatic(inc);

  DerivedGoodStruct dgs;
  auto dinc = dgs.badReturn();
  dgs.badArg(dinc);
  DerivedGoodStruct::badStatic(dinc);

  UsingGoodStruct ugs;
  auto uinc = ugs.badReturn();
  ugs.badArg(uinc);
  UsingGoodStruct::badStatic(uinc);
}

//--- ok.swift
import Function


func ok() {
  let _: UnsafePointer<GoodStruct>
  let _: UnsafePointer<DerivedGoodStruct>
  let _: UnsafePointer<UsingGoodStruct>
  let _: UnsafePointer<UsesGoodStruct>

  let gs = GoodStruct() // expected-warning {{never used}}
  // TODO: calling overloadsDiffNumArgs(_:_) should work since we should never
  //       need to import the (invalid) single-argument overload. The following
  //       call is commented out so that ok.swift compiles without C++ errors,
  //       but should be added back when this behavior is fixed.
  // gs.overloadsDiffNumArgs(42, 24)

  let _ = DerivedGoodStruct()
  let _ = UsingGoodStruct()

  let usgs = UsesGoodStruct()
  var f = usgs.field
  f = usgs.returns()
  usgs.takes(f)
}

//--- err.swift
import Function

func err() {
  let gs = GoodStruct()
  let inc = gs.badReturn()  // expected-swift-error {{is unavailable}}
                            // expected-swift-warning@-1 {{an enum with no cases}}
                            // expected-swift-note@-2 {{add an explicit type annotation}}
  gs.badArg(inc)            // expected-swift-error {{has no member}}
  GoodStruct.badStatic(inc) // expected-swift-error {{has no member}}

  let _ = GoodStruct(inc) // expected-swift-error {{call that takes no arguments}}

  // TODO: overload sets are looked up and imported as a group, so even the
  //       valid overloads of the following methods are missing due to invalid
  //       members in the overload set
  gs.overloadsSameNumArgs(42)
  gs.overloadsSameNumArgs(inc) // expected-swift-error {{cannot convert value of type 'Never'}}

  gs.overloadsDiffNumArgs(42, 24)
  gs.overloadsDiffNumArgs(inc) // expected-swift-error {{cannot convert value of type 'Never'}}
                               // expected-swift-error@-1 {{missing argument for parameter #2 in call}}

  let dgs = DerivedGoodStruct()
  let dinc = dgs.badReturn()  // expected-swift-error {{is unavailable}}
                              // expected-swift-warning@-1 {{an enum with no cases}}
                              // expected-swift-note@-2 {{add an explicit type annotation}}
  dgs.badArg(dinc)            // expected-swift-error {{has no member}}

  let ugs = UsingGoodStruct()
  let uinc = ugs.badReturn() // expected-swift-error {{is unavailable}}
                             // expected-swift-warning@-1 {{an enum with no cases}}
                             // expected-swift-note@-2 {{add an explicit type annotation}}
  ugs.badArg(uinc)           // expected-swift-error {{has no member}}
}

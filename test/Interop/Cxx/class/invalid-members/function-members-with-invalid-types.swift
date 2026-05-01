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
// RUN:   -typo-correction-limit 0 \
// RUN:   %t%{fs-sep}ok.swift -verify-additional-prefix ok-swift-
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}function.h \
// RUN:   -typo-correction-limit 0 \
// RUN:   %t%{fs-sep}err.swift -verify-additional-prefix swift-
//
// Note that we need -typo-correction-limit 0 because otherwise the compiler
// will try to import other members anyway to suggest "did you mean" diagnostics
// that unintentionally trigger even more instantiations and thus errors.
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

struct __attribute__((swift_attr("import_reference")))
       __attribute__((swift_attr("retain:immortal")))
       __attribute__((swift_attr("release:immortal"))) FRT {};

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

  // expected-swift-note@+1 * {{explicitly marked unavailable here}}
  Bro<Ken> getBad() const;

  // expected-swift-note@+2 {{unavailable (cannot import)}}
  // expected-swift-note@+1 {{unavailable (cannot import)}}
  static Bro<Ken> badStatic(Bro<Ken>);

  // expected-swift-note@+1 * {{unavailable (cannot import)}}
  virtual Bro<Ken> badVirtual(Bro<Ken>);

  void overloadsSameNumArgs(int) const;
  void overloadsSameNumArgs(Bro<Ken>) const;

  // expected-swift-note@+1 {{declared here}}
  void overloadsDiffNumArgs(int, int) const;
  void overloadsDiffNumArgs(Bro<Ken>) const;

  // FIXME: operators are imported eagerly, so having this member would make GoodStruct unusable
  // int operator+(Bro<Ken>) const;

  Bro<Ken> begin() const;
  Bro<Ken> end() const;

  // Return type templates are instantiated only if the function member is
  // actually imported, e.g., not if the function member is deleted.
  Bro<Ken> operator~() const = delete;
  Bro<Ken> deleted() const = delete;
  Bro<Ken> frtArgByValue(FRT) const; // expected-ok-swift-note {{uses foreign reference type 'FRT' as a value}}
};
// CHECK:      struct GoodStruct {
// CHECK-NEXT:   init()
//
// CHECK-NEXT:   func badReturn() -> Never
//
// NOTE-MISSING: func badArg(_: Never)
//
// CHECK-NEXT:   func getBad() -> Never
//
// NOTE-MISSING: func badStatic(_: Never) -> Never
//
// NOTE-MISSING: func badVirtual(_: Never) -> Never
//
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// NOTE-MISSING: func overloadsSameNumArgs(_: Never)
//
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// NOTE-MISSING: func overloadsDiffNumArgs(_: Never)
//
// CHECK-NEXT:   func __beginUnsafe() -> Never
// CHECK-NEXT:   func __endUnsafe() -> Never
// CHECK-NEXT: }


struct DerivedGoodStruct : GoodStruct {};
// CHECK:      struct DerivedGoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func badReturn() -> Never
// CHECK-NEXT:   func getBad() -> Never
// NOTE-MISSING: func badVirtual(_: Never) -> Never
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// CHECK-NEXT:   func __beginUnsafe() -> Never
// CHECK-NEXT:   func __endUnsafe() -> Never
// CHECK-NEXT: }

struct UsingGoodStruct : GoodStruct {
  using GoodStruct::badReturn;
  using GoodStruct::badArg;
  using GoodStruct::getBad;
  using GoodStruct::badStatic;
};

// CHECK:      struct UsingGoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func badReturn() -> Never
// CHECK-NEXT:   func getBad() -> Never
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// CHECK-NEXT:   func __beginUnsafe() -> Never
// CHECK-NEXT:   func __endUnsafe() -> Never
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
  auto inc2 = gs.getBad();
  GoodStruct::badStatic(inc2);
  gs.badVirtual(inc);

  DerivedGoodStruct dgs;
  auto dinc = dgs.badReturn();
  dgs.badArg(dinc);
  auto dinc2 = dgs.getBad();
  DerivedGoodStruct::badStatic(dinc2);
  dgs.badVirtual(dinc);

  UsingGoodStruct ugs;
  auto uinc = ugs.badReturn();
  ugs.badArg(uinc);
  auto uinc2 = ugs.getBad();
  UsingGoodStruct::badStatic(uinc2);
}

//--- ok.swift
import Function


func ok() {
  let _: UnsafePointer<GoodStruct>
  let _: UnsafePointer<DerivedGoodStruct>
  let _: UnsafePointer<UsingGoodStruct>
  let _: UnsafePointer<UsesGoodStruct>

  let gs = GoodStruct()
  // TODO: calling overloadsDiffNumArgs(_:_) should work since we should never
  //       need to import the (invalid) single-argument overload. The following
  //       call is commented out so that ok.swift compiles without C++ errors,
  //       but should be added back when this behavior is fixed.
  // gs.overloadsDiffNumArgs(42, 24)

  // NOTE: these function members have invalid return types but aren't imported,
  // so attempting to use them should ONLY trigger missing member diagnostics
  // and no template instantiation errors.
  let _ = ~gs  // expected-error {{cannot be applied}}
  gs.deleted() // expected-error {{has no member}}
  gs.frtArgByValue() // expected-error {{has no member}}

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

  let inc2 = gs.getBad()     // expected-swift-error {{is unavailable}}
                             // expected-swift-warning@-1 {{an enum with no cases}}
                             // expected-swift-note@-2 {{add an explicit type annotation}}
  GoodStruct.badStatic(inc2) // expected-swift-error {{has no member}}
  gs.badVirtual(inc)        // expected-swift-error {{has no member}}

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

  let _ = dgs.getBad()  // expected-swift-error {{is unavailable}}

  dgs.badVirtual(dinc)        // expected-swift-error {{has no member}}

  let ugs = UsingGoodStruct()
  let uinc = ugs.badReturn() // expected-swift-error {{is unavailable}}
                             // expected-swift-warning@-1 {{an enum with no cases}}
                             // expected-swift-note@-2 {{add an explicit type annotation}}
  ugs.badArg(uinc)           // expected-swift-error {{has no member}}

  let _ = ugs.getBad()  // expected-swift-error {{is unavailable}}
}

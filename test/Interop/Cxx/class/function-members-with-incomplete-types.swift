// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Validate usability of function.h in C++ with clang
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/ok.cpp
// RUN: %target-clang -c -o /dev/null -Xclang -verify=cxx -I %t/Inputs %t/err.cpp
//
// Compare usability of function.h in Swift with swift-frontend
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}function.h %t%{fs-sep}ok.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}function.h %t%{fs-sep}err.swift -verify-additional-prefix swift-
//
// Check module interface of function.h
// RUN: %target-swift-ide-test -print-module -module-to-print=Function -I %t/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

//--- Inputs/module.modulemap
module Function {
  header "function.h"
  requires cplusplus
}

//--- Inputs/function.h
#pragma once

struct Incomplete;
// cxx-note@-1 {{forward declaration of}}
// cxx-note@-2 {{forward declaration of}}
// cxx-note@-3 {{forward declaration of}}

// None of GoodStruct's members should prevent it from being imported sans error
struct GoodStruct {
  GoodStruct() = default;
  GoodStruct(Incomplete);

  // expected-swift-note@+3 {{explicitly marked unavailable here}}
  // expected-swift-note@+2 {{explicitly marked unavailable here}}
  // expected-swift-note@+1 {{explicitly marked unavailable here}}
  Incomplete badReturn() const;
  // cxx-note@-1 {{declared here}}
  // cxx-note@-2 {{declared here}}
  // cxx-note@-3 {{declared here}}

  // expected-swift-note@+2 {{unavailable (cannot import)}}
  // expected-swift-note@+1 {{unavailable (cannot import)}}
  void badArg(Incomplete) const;

  // expected-swift-note@+2 {{unavailable (cannot import)}}
  // expected-swift-note@+1 {{unavailable (cannot import)}}
  static Incomplete badStatic(Incomplete);

  void overloadsSameNumArgs(int) const;
  void overloadsSameNumArgs(Incomplete) const;

  // expected-swift-note@+1 {{declared here}}
  void overloadsDiffNumArgs(int, int) const;
  void overloadsDiffNumArgs(Incomplete) const;

  int operator+(Incomplete) const;

  Incomplete begin() const;
  Incomplete end() const;
};
// CHECK:      struct GoodStruct {
// CHECK-NEXT:   init()
//
// CHECK-NEXT:   @available(*, unavailable, message: "return type is unavailable in Swift")
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
// NOTE-MISSING: static func +(lhs: GoodStruct, rhs: Never) -> Int32
//
// CHECK-NEXT:   @available(*, unavailable, message: "return type is unavailable in Swift")
// CHECK-NEXT:   func __beginUnsafe() -> Never
// CHECK-NEXT:   @available(*, unavailable, message: "return type is unavailable in Swift")
// CHECK-NEXT:   func __endUnsafe() -> Never
// CHECK-NEXT: }


struct DerivedGoodStruct : GoodStruct {};
// CHECK:      struct DerivedGoodStruct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func badReturn() -> Never
// CHECK-NEXT:   func overloadsSameNumArgs(_: Int32)
// CHECK-NEXT:   func overloadsDiffNumArgs(_: Int32, _: Int32)
// CHECK-NEXT:   func __beginUnsafe() -> Never
// CHECK-NEXT:   func __endUnsafe() -> Never
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
  auto inc = gs.badReturn(); // cxx-error {{with incomplete return type}}
  gs.badArg(inc);
  GoodStruct::badStatic(inc);

  DerivedGoodStruct dgs;
  auto dinc = dgs.badReturn(); // cxx-error {{with incomplete return type}}
  dgs.badArg(dinc);
  DerivedGoodStruct::badStatic(dinc);

  UsingGoodStruct ugs;
  auto uinc = ugs.badReturn(); // cxx-error {{with incomplete return type}}
  ugs.badArg(uinc);
  UsingGoodStruct::badStatic(uinc);
}

//--- ok.swift
import Function

func ok() {
  let gs = GoodStruct()
  gs.overloadsSameNumArgs(42)
  gs.overloadsDiffNumArgs(42, 24)

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

  gs.overloadsSameNumArgs(42)
  gs.overloadsSameNumArgs(inc) // expected-swift-error {{cannot convert value of type 'Never'}}

  gs.overloadsDiffNumArgs(42, 24)
  gs.overloadsDiffNumArgs(inc) // expected-swift-error {{cannot convert value of type 'Never'}}
                               // expected-swift-error@-1 {{missing argument for parameter #2 in call}}

  let _ = gs + inc    // expected-swift-error {{binary operator '+' cannot be applied}}

  let _ = gs.begin()  // expected-swift-error {{has no member}}
                      // expected-swift-note@-1 {{is unavailable}}
                      // expected-swift-note@-2 {{try using Swift collection APIs instead}}
  let _ = gs.end()    // expected-swift-error {{has no member}}
                      // expected-swift-note@-1 {{is unavailable}}
                      // expected-swift-note@-2 {{try using Swift collection APIs instead}}

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

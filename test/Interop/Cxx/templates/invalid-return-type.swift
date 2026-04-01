// RUN: split-file %s %t
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/cxx.cpp
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h \
// RUN:   %t%{fs-sep}ok.swift
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h \
// RUN:   %t%{fs-sep}err.swift -verify-additional-prefix err-
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -cxx-interoperability-mode=default -I %t/Inputs \
// RUN:   -module-to-print=CxxModule | %FileCheck %t/Inputs/CxxHeader.h
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once


template <class T> struct OnlyChar;
template <> struct OnlyChar<char> {};

struct S {
  S() = default;

  OnlyChar<int> Err() const; // cannot be called (incomplete return type)
  // expected-err-note@-1 {{explicitly marked unavailable here}}

  OnlyChar<char> Ok() const;
};

// CHECK:      struct S {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func Err() -> Never
// CHECK-NEXT:   func Ok()
// CHECK:      }

//--- cxx.cpp
// expected-no-diagnostics
#include <CxxHeader.h>
void IsOk(void) {
  S Is;
  Is.Ok();
}

//--- ok.swift
import CxxModule
func IsOk() {
  let Is = S()  // We can instantiate it
  Is.Ok()       // We can call it
}

//--- err.swift
import CxxModule
func IsErr() {
  let Is = S()  // We can instantiate it
  Is.Err()      // expected-error {{return type is unavailable in Swift}}
  Is.Ok()       // We can still call it
}

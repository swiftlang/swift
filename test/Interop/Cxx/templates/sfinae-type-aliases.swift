// RUN: split-file %s %t
//
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/cxx.cpp
//
// Check for absence of errors when we don't try to use invalid types
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h \
// RUN:   %t%{fs-sep}main.swift
//
// Check for presence of errors when we do try to use invalid types
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -verify-additional-prefix poke- \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h \
// RUN:   %t%{fs-sep}poke.swift
//
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -module-to-print=CxxModule -I %t/Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN: | %FileCheck %t/Inputs/CxxHeader.h

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once

struct Empty {}; // expected-poke-error@+1 {{no type named 'Missing' in 'Empty'}}
template <typename T> struct MissingMember { typename T::Missing member; };
using SUB = MissingMember<Empty>;

struct Incomplete; // expected-poke-error@+1 {{field has incomplete type 'Incomplete'}}
template <typename T> struct IncompleteField { T member; };
using INC = IncompleteField<Incomplete>;

struct Aliaser {
  typedef SUB  TypedefSUB;
  typedef INC  TypedefINC;
  typedef bool TypedefBool;
  using UsingSUB  = SUB;
  using UsingINC  = INC;
  using UsingBool = bool;
};
// CHECK:      struct Aliaser {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias TypedefBool = Bool
// CHECK-NEXT:   typealias UsingBool = Bool
// CHECK-NEXT: }

//--- cxx.cpp
// expected-no-diagnostics
#include <CxxHeader.h>
void make(void) {
  Aliaser _ {};
  Aliaser::TypedefBool __ = true;
  Aliaser::UsingBool ___ = false;
}

//--- main.swift
import CxxModule
func make() {
  let _: Aliaser = .init()
  let _: Aliaser.TypedefBool = true
  let _: Aliaser.UsingBool = false
}

//--- poke.swift
import CxxModule
func make(_ _: Aliaser.TypedefSUB,  // expected-poke-error {{'TypedefSUB' is not a member type of struct}}
          _ _: Aliaser.UsingINC) {} // expected-poke-error {{'UsingINC' is not a member type of struct}}

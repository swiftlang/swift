// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend   -typecheck -verify -I %t/Inputs %t/main.swift
// RUN: %target-swiftxx-frontend -typecheck -verify -I %t/Inputs %t/main.swift

// Import an anonymous union with const and non-const members. Constant members
// appear within Foo as clang::IndirectFieldDecl and should be imported in Swift
// as computed properties with private setters (since it is possible to change
// the values of const members in C/C++).

//--- Inputs/module.modulemap
module CxxModule { header "header.h" }

//--- Inputs/header.h
#pragma once

struct Foo {
  union {
    const int constant;
    int variable;
  };
};

//--- main.swift
import CxxModule

func fooer(_ f: inout Foo) {
  let _ = f.variable
  let _ = f.constant
  f.variable = 42
  f.constant = 42 // expected-error {{setter is inaccessible}}
}

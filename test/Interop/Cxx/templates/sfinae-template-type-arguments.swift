// RUN: split-file %s %t
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/cxx.cpp
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxHeader.h %t%{fs-sep}main.swift
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxModule -I %t/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %t/Inputs/CxxHeader.h

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "CxxHeader.h"
}

//--- Inputs/CxxHeader.h
#pragma once

struct Empty {};
template <typename T> struct MissingMember { typename T::Missing member; };
using SUB = MissingMember<Empty>;

struct Incomplete;
template <typename T> struct IncompleteField { T member; };
using INC = IncompleteField<Incomplete>;

template <typename S = SUB, typename I = INC> struct DefaultedTemplateArgs {};
using DefaultedTemplateArgsInst = DefaultedTemplateArgs<>;

// CHECK: struct DefaultedTemplateArgs<MissingMember<Empty>, IncompleteField<Incomplete>> {
// CHECK: }
// CHECK: typealias DefaultedTemplateArgsInst = DefaultedTemplateArgs<MissingMember<Empty>, IncompleteField<Incomplete>>

template <typename S, typename I> struct ThrowsAwayTemplateArgs {};
using ThrowsAwayTemplateArgsInst = ThrowsAwayTemplateArgs<SUB, INC>;

// CHECK: struct ThrowsAwayTemplateArgs<MissingMember<Empty>, IncompleteField<Incomplete>> {
// CHECK: }
// CHECK: typealias ThrowsAwayTemplateArgsInst = ThrowsAwayTemplateArgs<MissingMember<Empty>, IncompleteField<Incomplete>>

//--- cxx.cpp
// expected-no-diagnostics
#include <CxxHeader.h>
void make(void) {
  DefaultedTemplateArgs<>   dta{};
  DefaultedTemplateArgsInst dtai{};

  ThrowsAwayTemplateArgs<SUB, INC>  tata{};
  ThrowsAwayTemplateArgsInst        tatai{};
}

//--- main.swift
import CxxModule
func make() {
  let _: DefaultedTemplateArgsInst = .init()
  let _: ThrowsAwayTemplateArgsInst = .init()
}

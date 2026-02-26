// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}header.h \
// RUN:   %t%{fs-sep}test.swift
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

//--- Inputs/module.modulemap
module TheHeader {
  header "header.h"
  requires cplusplus
}

//--- Inputs/header.h
#pragma once
#include <type_traits>

template <typename T>
struct NeedsInt {
    static_assert(std::is_same_v<T, int>, "must be instantiated with int");
};

struct GivesDoubleToNeedsInt {
    typedef NeedsInt<double> bad_use_of_needs_int;
};

typedef GivesDoubleToNeedsInt TypedefForGives;
struct SomeDataStructure {
    TypedefForGives SomeMethod() const;

    struct IteratorForSomeDataStructure {
        using value_type = const SomeDataStructure;
        IteratorForSomeDataStructure operator+(const int increment) const;
    };
};

//--- test.swift
import TheHeader

func ok() {
  let _ = SomeDataStructure.IteratorForSomeDataStructure()
  let a: [Bool] = []
  let b: [Bool] = []
  let _ = a + b
}

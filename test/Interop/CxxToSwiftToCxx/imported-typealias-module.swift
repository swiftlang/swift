// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/use-alias.swift -module-name UseAlias -typecheck -verify -I %t -cxx-interoperability-mode=default -emit-clang-header-nonmodular-includes -emit-clang-header-path %t/UseAlias.h
// RUN: %FileCheck %s --implicit-check-not=unused.h --implicit-check-not='module import CxxAliases.Unused' < %t/UseAlias.h
// RUN: %check-interop-cxx-header-in-clang(%t/UseAlias.h -I %t)
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only -x c++-header %t/UseAlias.h -I %t -fmodules -fcxx-modules -fmodules-cache-path=%t/module-cache
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only -x c++-header %t/UseAlias.h -I %t -fmodules -fcxx-modules -fmodules-cache-path=%t/module-cache
// RUN: %target-swift-frontend %t/use-nominal.swift -module-name UseNominal -typecheck -verify -I %t -cxx-interoperability-mode=default -emit-clang-header-nonmodular-includes -emit-clang-header-path %t/UseNominal.h
// RUN: %FileCheck %s --check-prefix=NOMINAL < %t/UseNominal.h
// RUN: %check-interop-cxx-header-in-clang(%t/UseNominal.h -I %t)

// The alias belongs to an explicit submodule, separately from its underlying
// template. No nominal type from either module appears in the Swift interface
// to register the dependency on behalf of the alias.
// CHECK: #pragma clang module import CxxAliases.Used
// CHECK: #include <aliases.h>
// CHECK: isUsableInGenericContext<test::Alias> = true;
// CHECK: SWIFT_INLINE_THUNK test::Box<int> identity(const test::Box<int>& value)

// A namespace member's Swift declaration context is the synthetic imported
// header module. Both aliases and nominal types need their actual Clang owner.
// NOMINAL: #pragma clang module import CxxTypes
// NOMINAL: #include <types.h>
// NOMINAL: SWIFT_INLINE_THUNK test::Record identity(const test::Record& value)

//--- module.modulemap
module CxxTypes {
  header "types.h"
  requires cplusplus
  export *
}
module CxxAliases {
  requires cplusplus
  explicit module Used {
    header "aliases.h"
    export *
  }
  explicit module Unused {
    header "unused.h"
  }
}

//--- types.h
#pragma once
namespace test {
template<class T> struct Box { T value; };
struct Record { int value; };
}

//--- aliases.h
#pragma once
#include "types.h"
namespace test {
using Alias = Box<int>;
}

//--- unused.h
#pragma once
struct Unused {};

//--- use-alias.swift
import CxxAliases.Used
import CxxAliases.Unused

public func identity(_ value: test.Alias) -> test.Alias { value }

//--- use-nominal.swift
import CxxTypes

public func identity(_ value: test.Record) -> test.Record { value }

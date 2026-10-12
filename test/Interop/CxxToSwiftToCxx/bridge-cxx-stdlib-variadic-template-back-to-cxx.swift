// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/use-templates.swift -module-name UseTemplates -typecheck -verify -emit-clang-header-path %t/UseTemplates.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -Xcc -std=c++17
// RUN: %FileCheck %s --input-file %t/UseTemplates.h
// RUN: %target-interop-build-clangxx -fsyntax-only -x c++-header -std=c++17 -include %t/templates.h %t/UseTemplates.h
// RUN: %target-interop-build-clangxx -fsyntax-only -x c++-header -std=c++20 -include %t/templates.h %t/UseTemplates.h

//--- templates.h
#include <tuple>
#include <variant>

using Variant = std::variant<int, double>;
using Tuple = std::tuple<int, double>;
// Instantiate the aliased specializations so they are available to Swift.
inline Variant makeVariantAlias() { return 42; }
inline Tuple makeTupleAlias() { return {42, 3.5}; }

inline std::variant<double, int> makeVariant() { return 3.5; }
inline std::tuple<double, int> makeTuple() { return {3.5, 42}; }

//--- module.modulemap
module CxxTemplates {
  header "templates.h"
  requires cplusplus
}

//--- use-templates.swift
import CxxTemplates

public func variantAlias(_ value: Variant) -> Variant { value }
public func tupleAlias(_ value: Tuple) -> Tuple { value }

// Infer these types from functions returning specializations without aliases.
public struct Direct {
  public let variant = makeVariant()
  public let tuple = makeTuple()
}

// CHECK: SWIFT_INLINE_THUNK std::{{(__[A-Za-z0-9_]+::)?}}tuple<int, double> tupleAlias(const std::{{(__[A-Za-z0-9_]+::)?}}tuple<int, double>& value)
// CHECK: SWIFT_INLINE_THUNK std::{{(__[A-Za-z0-9_]+::)?}}variant<int, double> variantAlias(const std::{{(__[A-Za-z0-9_]+::)?}}variant<int, double>& value)
// CHECK: SWIFT_INLINE_THUNK std::{{(__[A-Za-z0-9_]+::)?}}variant<double, int> Direct::getVariant() const
// CHECK: SWIFT_INLINE_THUNK std::{{(__[A-Za-z0-9_]+::)?}}tuple<double, int> Direct::getTuple() const

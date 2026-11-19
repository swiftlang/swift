// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/extensions.swift -module-name TemplateExtensions -I %t/Inputs -o %t/TemplateExtensions.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t/Inputs -I %t -cxx-interoperability-mode=default -verify

//--- Inputs/module.modulemap
module TemplateTypes {
  header "templates.h"
  requires cplusplus
}
module TemplateReexports {
  requires cplusplus
  explicit module Types {
    header "reexport.h"
    export *
  }
}
module TemplateAliases {
  header "aliases.h"
  requires cplusplus
}
module TemplateSubmodules {
  requires cplusplus
  module Aliases {
    header "submodule-aliases.h"
  }
}

//--- Inputs/templates.h
#pragma once

template <class T> struct Box { int marker = 0; };
using IntBox = Box<int>;
using DoubleBox = Box<double>;

namespace Nested {
template <class T> struct Box { int marker = 0; };
}
using NestedIntBox = Nested::Box<int>;

struct PlainRecord { int marker = 0; };

// The specialization is named before the template is defined. This header
// names it first, so it belongs to TemplateTypes even though its alias is in
// aliases.h.
template <class T> struct ForwardDeclared;
ForwardDeclared<int> makeForwardDeclared();
template <class T> struct ForwardDeclared { int marker = 0; };

// These specializations are only named in other modules, so they belong to
// those modules rather than to TemplateTypes.
template <class T> struct OnlyAliased { int marker = 0; };
template <class T> struct SubmoduleAliased { int marker = 0; };

//--- Inputs/reexport.h
#pragma once
#include "templates.h"

//--- Inputs/aliases.h
#pragma once
// TemplateAliases imports TemplateTypes without re-exporting it. It also
// reaches the templates through TemplateReexports.Types, so searching its
// imports finds OnlyAliased twice.
#include "templates.h"
#include "reexport.h"
using ForwardDeclaredInt = ForwardDeclared<int>;
using OnlyAliasedInt = OnlyAliased<int>;

//--- Inputs/submodule-aliases.h
#pragma once
// Only the explicit submodule TemplateReexports.Types re-exports the
// templates here.
#include "reexport.h"
using SubmoduleAliasedInt = SubmoduleAliased<int>;

//--- extensions.swift
import TemplateTypes
import TemplateAliases
import TemplateSubmodules

extension IntBox {
  public typealias Value = String
  public func echo(_ value: Value) -> Value { value }
}

extension DoubleBox {
  public typealias Value = Double
  public func echo(_ value: Value) -> Value { value }
}

// A specialization inside a namespace uses a nested cross-reference path.
extension NestedIntBox {
  public typealias Value = Bool
  public func echo(_ value: Value) -> Value { value }
}

extension PlainRecord {
  public func probe() -> Int { 42 }
}

extension ForwardDeclaredInt {
  public func probe() -> Bool { true }
}

extension OnlyAliasedInt {
  public typealias Value = Int
  public func echo(_ value: Value) -> Value { value }
}

extension SubmoduleAliasedInt {
  public typealias Value = Float
  public func echo(_ value: Value) -> Value { value }
}

//--- client.swift
import TemplateTypes
import TemplateAliases
import TemplateSubmodules
import TemplateExtensions

func check(_ first: IntBox, _ second: DoubleBox, _ nested: NestedIntBox,
           _ plain: PlainRecord, _ forwardDeclared: ForwardDeclaredInt,
           _ onlyAliased: OnlyAliasedInt,
           _ submoduleAliased: SubmoduleAliasedInt) {
  let text: IntBox.Value = "value"
  let number: DoubleBox.Value = 2.5
  let boolean: NestedIntBox.Value = true
  let count: OnlyAliasedInt.Value = 1
  let ratio: SubmoduleAliasedInt.Value = 0.5
  let _: String = first.echo(text)
  let _: Double = second.echo(number)
  let _: Bool = nested.echo(boolean)
  let _: Int = plain.probe()
  let _: Bool = forwardDeclared.probe()
  let _: Int = onlyAliased.echo(count)
  let _: Float = submoduleAliased.echo(ratio)
}

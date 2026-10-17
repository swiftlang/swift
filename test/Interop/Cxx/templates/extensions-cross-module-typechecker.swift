// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/extensions.swift -module-name TemplateExtensions -I %t -o %t/TemplateExtensions.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -cxx-interoperability-mode=default -verify

//--- module.modulemap
module TemplateTypes {
  header "templates.h"
  requires cplusplus
}
module TemplateAliases {
  header "aliases.h"
  requires cplusplus
}

//--- templates.h
#pragma once

template <class T> struct Box { int marker = 0; };
using IntBox = Box<int>;
using DoubleBox = Box<double>;

namespace Nested {
template <class T> struct Box { int marker = 0; };
}
using NestedIntBox = Nested::Box<int>;

struct PlainRecord { int marker = 0; };

// The specialization is used before the definition, and its alias belongs to
// another Clang module. https://github.com/swiftlang/swift/issues/62165
template <class T> struct ForwardDeclared;
ForwardDeclared<int> makeForwardDeclared();
template <class T> struct ForwardDeclared { int marker = 0; };

//--- aliases.h
#pragma once
#include "templates.h"
using ForwardDeclaredInt = ForwardDeclared<int>;

//--- extensions.swift
import TemplateTypes
import TemplateAliases

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

//--- client.swift
import TemplateTypes
import TemplateAliases
import TemplateExtensions

func check(_ first: IntBox, _ second: DoubleBox, _ nested: NestedIntBox,
           _ plain: PlainRecord, _ forwardDeclared: ForwardDeclaredInt) {
  let text: IntBox.Value = "value"
  let number: DoubleBox.Value = 2.5
  let boolean: NestedIntBox.Value = true
  let _: String = first.echo(text)
  let _: Double = second.echo(number)
  let _: Bool = nested.echo(boolean)
  let _: Int = plain.probe()
  let _: Bool = forwardDeclared.probe()
}

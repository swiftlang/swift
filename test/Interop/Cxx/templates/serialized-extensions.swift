// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/protocols.swift -module-name Protocols -o %t/Protocols.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/extensions.swift -module-name Extensions -I %t -cxx-interoperability-mode=default -o %t/Extensions.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -cxx-interoperability-mode=default -verify

//--- module.modulemap
module Types {
  header "types.h"
  requires cplusplus
}
module Aliases {
  header "aliases.h"
  requires cplusplus
}

//--- types.h
#pragma once

template <class T> struct Box { int marker = 0; };
using IntBox = Box<int>;
using DoubleBox = Box<double>;

namespace Nested {
template <class T> struct Box { int marker = 0; };
using IntBox = Box<int>;
}

struct Plain { int marker = 0; };

template <class T> struct Forward;
Forward<int> getForward();
template <class T> struct Forward { int marker = 0; };

namespace swift { class String; }
template <class T>
struct __attribute__((swift_attr("conforms_to:Protocols.CxxPromise"))) Promise {
  using Value = T;
  int marker = 0;
};
using StringPromise = Promise<swift::String>;

//--- aliases.h
#include "types.h"
using ForwardInt = Forward<int>;

//--- protocols.swift
public protocol NativePromise<Value> {
  associatedtype Value
  func echo(_ value: Value) -> Value
}

public protocol CxxPromise<Value>: NativePromise {}

extension CxxPromise {
  public func echo(_ value: Value) -> Value { value }
}

//--- extensions.swift
public import Types
public import Aliases
public import Protocols

extension IntBox {
  public typealias Value = String
  public func value() -> Value { "int" }
}

extension DoubleBox {
  public typealias Value = Double
  public func value() -> Value { 1.5 }
}

extension Nested.IntBox {
  public func value() -> Int { 2 }
}

extension Plain {
  public func value() -> Int { 3 }
}

extension ForwardInt {
  public func value() -> Int { 4 }
}

extension StringPromise {
  public typealias Value = String
}

public func makePromise() -> any NativePromise<String> { StringPromise() }

//--- client.swift
import Types
import Aliases
import Protocols
import Extensions

func testExtensions(_ intBox: IntBox, _ doubleBox: DoubleBox,
                    _ nested: Nested.IntBox, _ plain: Plain,
                    _ forward: ForwardInt) {
  let _: IntBox.Value = "hello"
  let _: String = intBox.value()
  let _: DoubleBox.Value = 1.5
  let _: Double = doubleBox.value()
  let _: Int = nested.value()
  let _: Int = plain.value()
  let _: Int = forward.value()
}

func testConformance() {
  let promise: any NativePromise<String> = StringPromise()
  let _: String = promise.echo("hello")
  let _: String = makePromise().echo("hello")
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Core.swift -module-name Core -o %t/Core.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/A.swift -module-name A -I %t -o %t/A.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/B.swift -module-name B -I %t -o %t/B.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -cxx-interoperability-mode=default -verify
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -cxx-interoperability-mode=default -D IMPORT_B -verify

//--- module.modulemap
module Promises {
  header "promises.h"
  requires cplusplus
}

//--- promises.h
#pragma once
#include <swift/bridging>

namespace swift { class String; }

template <class T>
struct SWIFT_CONFORMS_TO_PROTOCOL(Core.CxxPromise) Promise {
  using Value = T;
  int marker = 0;
};

using StringPromise = Promise<swift::String>;
using DoublePromise = Promise<double>;

//--- Core.swift
public protocol NativePromise<Value> {
  associatedtype Value
  func echo(_ value: Value) -> Value
}

public protocol CxxPromise<Value>: NativePromise {}

extension CxxPromise {
  public func echo(_ value: Value) -> Value { value }
}

//--- A.swift
import Core
import Promises

// The annotation supplies the conformance. This extension only supplies the
// native associated type for the forward-declared C++ payload.
extension StringPromise {
  public typealias Value = String
}

public func makeA() -> any NativePromise<String> { StringPromise() }

//--- B.swift
import Core
import Promises

extension StringPromise {
  public typealias Value = String
}

public func makeB() -> any NativePromise<String> { StringPromise() }

//--- client.swift
import Core
import Promises
import A
#if IMPORT_B
import B
#endif

func check() {
  let direct: any NativePromise<String> = StringPromise()
  let text: StringPromise.Value = "value"
  let _: String = direct.echo(text)
  let _: String = makeA().echo(text)
  let _: any NativePromise<Double> = DoublePromise()
#if IMPORT_B
  let _: String = makeB().echo(text)
#endif
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/retroactive_conformance_other.swiftmodule %S/Inputs/retroactive_conformance_other.swift
// RUN: %target-swift-frontend -emit-ir %s -I %t

import retroactive_conformance_other

public struct Foo<T: P> {}

public enum MyError : Error {
  case a
}

extension E: P where First: P {
}

public enum Bar<T: P> {
  case x(Foo<E<T, MyError>>)
  case y
}



// RUN: %empty-directory(%t)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -disable-objc-interop -emit-module -enable-library-evolution -swift-version 5 -module-name Lib %t/src/Lib.swift -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -disable-objc-interop -typecheck -swift-version 5 -I %t -verify %t/src/Client.swift

// A @c enum from a library-evolution module is only exhaustive if @frozen.

//--- Lib.swift
@c public enum CEnum: Int8 { case a, b }
@c @frozen public enum FrozenCEnum: Int8 { case a, b }

//--- Client.swift
import Lib

func f(_ e: CEnum) -> Int {
  switch e { // expected-warning {{switch covers known cases, but 'CEnum' may have additional unknown values}}
  // expected-note@-1 {{handle unknown values using "@unknown default"}}
  case .a: return 0
  case .b: return 1
  }
}

func g(_ e: FrozenCEnum) -> Int {
  switch e {
  case .a: return 0
  case .b: return 1
  }
}

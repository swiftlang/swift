// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Building a module from this interface should always succeed, even though
// ParserRoundTrip is an experimental feature that is not enabled in production
// compilers.

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t
// RUN: %target-swift-frontend -compile-module-from-interface -module-name ExperimentalFeatures -o /dev/null %t/ExperimentalFeatures.swiftinterface -verify
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name ExperimentalFeatures %t/ExperimentalFeatures.swiftinterface -verify

//--- ExperimentalFeatures.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name ExperimentalFeatures -enable-experimental-feature ParserRoundTrip

import Swift
extension Int {
  public static var fortytwo: Int = 42
}

//--- Client.swift

import ExperimentalFeatures

_ = Int.fortytwo

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ShadowsConcur.swiftmodule -module-name ShadowsConcur %S/Inputs/ShadowsConcur.swift
// RUN: %target-typecheck-verify-swift -I %t -enable-experimental-concurrency
// REQUIRES: concurrency


import ShadowsConcur

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func f(_ t : Task) -> Bool {
  return t.someProperty == "123"
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func g(_: _Concurrency.Task) {}

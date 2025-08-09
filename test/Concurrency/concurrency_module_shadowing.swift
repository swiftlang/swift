// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ShadowsConcur.swiftmodule -module-name ShadowsConcur %S/Inputs/ShadowsConcur.swift

// RUN: %target-swift-frontend -I %t  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -I %t  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -I %t  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

import ShadowsConcur

@available(SwiftStdlib 5.1, *)
func f(_ t : UnsafeCurrentTask) -> Bool {
  return t.someProperty == "123"
}

@available(SwiftStdlib 5.1, *)
func g(_: _Concurrency.UnsafeCurrentTask) {}

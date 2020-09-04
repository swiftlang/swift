// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_async~partial.swiftmodule -primary-file %S/Inputs/def_async.swift -module-name def_async -enable-experimental-concurrency
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_async~partial.swiftmodule -module-name def_async -o %t/def_async.swiftmodule -enable-experimental-concurrency
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown -enable-experimental-concurrency

import def_async

func testDoSomethingBig() {
  let _: () -> Int = doSomethingBig // expected-error{{cannot convert value of type '() async -> Int' to specified type '() -> Int'}}
}

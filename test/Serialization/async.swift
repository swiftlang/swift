// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_async~partial.swiftmodule -primary-file %S/Inputs/def_async.swift -module-name def_async  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_async~partial.swiftmodule -module-name def_async -o %t/def_async.swiftmodule  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

import def_async

func testDoSomethingBig() {
  let _: () -> Int = doSomethingBig // expected-error{{invalid conversion from 'async' function of type '() async -> Int' to synchronous function type '() -> Int'}}
}

// Verify serialization of async let _ = <expression> pattern
func testSerializedAsyncLet() async {
  await doSerializedAsyncLet()
} 

// Verify serialization of async let _ : <type> = <expression> pattern
func testSerializedAsyncLetTyped() async {
  await doSerializedAsyncLetTyped()
}

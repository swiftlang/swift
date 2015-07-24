// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

import cvars

func getPI() -> Float {
  return PI
}

func testPointers() {
  let cp = globalConstPointer
  cp.abcde() // expected-error {{'UnsafePointer<Void>' (aka 'UnsafePointer<()>') does not have a member named 'abcde'}}
  let mp = globalPointer
  mp.abcde() // expected-error {{'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>') does not have a member named 'abcde'}}
}

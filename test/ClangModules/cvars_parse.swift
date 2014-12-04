// RUN: %swift %clang-importer-sdk -parse -verify -target x86_64-apple-macosx10.9 %s

import cvars

func getPI() -> Float {
  return PI
}

func testPointers() {
  let cp = globalConstPointer
  cp.abcde() // expected-error {{'UnsafePointer<Void>' does not have a member named 'abcde'}}
  let mp = globalPointer
  mp.abcde() // expected-error {{'UnsafeMutablePointer<Void>' does not have a member named 'abcde'}}
}

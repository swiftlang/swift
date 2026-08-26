// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// expected-warning@<unknown> * {{libc not found for }}

import cvars

func getPI() -> Float {
  return PI
}

func testPointers() {
  let cp = globalConstPointer
  cp.abcde() // expected-error {{value of type 'UnsafeRawPointer?' has no member 'abcde'}}
  let mp = globalPointer
  mp.abcde() // expected-error {{value of type 'UnsafeMutableRawPointer?' has no member 'abcde'}}
}

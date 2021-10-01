// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-clang-importer-diagnostics 

import ctypes

func testPartialStructImport() {
  let s: PartialImport
  s.c = 5 // expected-error {{'c' is unavailable: 'Complex' is unavailable in Swift}}
  s.d = 5 // expected-error {{'d' is unavailable: 'Atomic' is unavailable in Swift}}
  partialImport.c = 5 // expected-error {{'c' is unavailable: 'Complex' is unavailable in Swift}}
  partialImport.d = 5 // expected-error {{'d' is unavailable: 'Atomic' is unavailable in Swift}}
  var newPartialImport = PartialImport()
  newPartialImport.a = 5
  newPartialImport.b = 5
  newPartialImport.c = 5  // expected-error {{'c' is unavailable: 'Complex' is unavailable in Swift}}
  newPartialImport.d = 5  // expected-error {{'d' is unavailable: 'Atomic' is unavailable in Swift}}
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -import-objc-header %S/Inputs/bad-ns-extensible-string-enum.h -verify
// REQUIRES: objc_interop

let string = MyString.MyStringOne // expected-error {{use of unresolved identifier 'MyString'}}

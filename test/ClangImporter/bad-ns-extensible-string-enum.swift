// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/bad-ns-extensible-string-enum.h -verify

let string = MyString.MyStringOne // expected-error {{use of unresolved identifier 'MyString'}}

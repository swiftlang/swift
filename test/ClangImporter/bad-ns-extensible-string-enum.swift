// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/bad-ns-extensible-string-enum.h -verify -verify-ignore-unrelated

let string = MyString.MyStringOne // expected-error {{cannot find 'MyString' in scope}}

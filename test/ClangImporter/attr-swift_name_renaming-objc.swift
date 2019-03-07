// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -I %S/Inputs/custom-modules -Xcc -w -typecheck -verify %s

import SwiftName

func test() {
  // This particular instance method mapping previously caused a crash because
  // of the trailing closure.
  acceptsClosure(Foo(), test) // expected-error {{'acceptsClosure' has been replaced by instance method 'Foo.accepts(closure:)'}} {{3-17=(Foo()).accepts}} {{18-25=}} {{25-25=closure: }}
  acceptsClosure(Foo()) {} // expected-error {{'acceptsClosure' has been replaced by instance method 'Foo.accepts(closure:)'}} {{3-17=(Foo()).accepts}} {{18-23=}}

  Foo().accepts(closure: test)
  Foo().accepts() {}
  Foo().accepts {}

  acceptsClosureStatic(test) // expected-error {{'acceptsClosureStatic' has been replaced by 'Foo.accepts(closure:)'}} {{3-23=Foo.accepts}}
  acceptsClosureStatic() {} // expected-error {{'acceptsClosureStatic' has been replaced by 'Foo.accepts(closure:)'}} {{3-23=Foo.accepts}}
  acceptsClosureStatic {} // expected-error {{'acceptsClosureStatic' has been replaced by 'Foo.accepts(closure:)'}} {{3-23=Foo.accepts}}

  Foo.accepts(closure: test)
  Foo.accepts() {}
  Foo.accepts {}

  _ = AnonymousEnumConstantObjC // expected-error {{'AnonymousEnumConstantObjC' has been renamed to 'Foo.anonymousEnumConstant'}}
  _ = Foo.anonymousEnumConstant // okay
}

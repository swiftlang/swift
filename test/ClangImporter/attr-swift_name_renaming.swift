// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -Xcc -w -typecheck -verify %s

// XFAIL: linux

import SwiftName

func test() {
  // Function name remapping
  drawString("hello", x: 3, y: 5)
  drawString("hello", 3, 5) // expected-error{{missing argument labels 'x:y:' in call}}

  // Enum name remapping.
  var color: ColorKind = CT_red
  var color2: ColorType = CT_Red // expected-error{{'ColorType' has been renamed to 'ColorKind'}}{{15-24=ColorKind}}

  // Enumerator remapping.
  var excuse: HomeworkExcuse = .dogAteIt
  excuse = .overslept // FIXME: should provide Fix-It  expected-error{{type 'HomeworkExcuse' has no member 'overslept'}} {{none}}
  excuse = .tired
  excuse = .tooHard // FIXME: should provide Fix-It expected-error{{type 'HomeworkExcuse' has no member 'tooHard'}} {{none}}
  excuse = .challenging

  // Typedef-of-anonymous-type-name renaming
  var p = Point()
  var p2 = PointType() // FIXME: should provide Fix-It expected-error{{use of unresolved identifier 'PointType'}} {{none}}

  // Field name remapping
  p.x = 7

  // Typedef renaming
  var mi: MyInt = 5
  var mi2: my_int_t = 7 // expected-error{{'my_int_t' has been renamed to 'MyInt'}}{{12-20=MyInt}}

  spuriousAPINotedSwiftName(0)
  nicelyRenamedFunction("go apinotes!")

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
}

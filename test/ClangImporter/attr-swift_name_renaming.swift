// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -Xcc -w -typecheck -verify %s

import SwiftName

func test() {
  // Function name remapping
  drawString("hello", x: 3, y: 5)
  drawString("hello", 3, 5) // expected-error{{missing argument labels 'x:y:' in call}}

  // Enum name remapping.
  var color: ColorKind = CT_red
  var color2: ColorType = CT_red // expected-error{{'ColorType' has been renamed to 'ColorKind'}}{{15-24=ColorKind}}

  // Enumerator remapping.
  var excuse: HomeworkExcuse = .dogAteIt
  excuse = .overslept // expected-error{{type 'HomeworkExcuse' has no member 'overslept'; did you mean 'Overslept'?}} {{13-22=Overslept}}
  excuse = .tired
  excuse = .tooHard // expected-error{{type 'HomeworkExcuse' has no member 'tooHard'; did you mean 'TooHard'?}} {{13-20=TooHard}}
  excuse = .challenging

  // Typedef-of-anonymous-type-name renaming
  var p = Point()
  var p2 = PointType() // FIXME: should provide Fix-It expected-error{{cannot find 'PointType' in scope}} {{none}}

  // Initializers with incorrect argument count (rdar://141124373)
  var p3 = Point(path: "/dev/zero", nil) // expected-warning {{'init(path:_:)' is deprecated: declared Swift name 'init(path:)' was adjusted to 'init(path:_:)' because it does not have the correct number of parameters (1 vs. 2); please report this to its maintainer}}

  // Field name remapping
  p.x = 7

  // Typedef renaming
  var mi: MyInt = 5
  var mi2: my_int_t = 7 // expected-error{{'my_int_t' has been renamed to 'MyInt'}}{{12-20=MyInt}}

  spuriousAPINotedSwiftName(0)
  nicelyRenamedFunction("go apinotes!")

  _ = AnonymousEnumConstant // expected-error {{'AnonymousEnumConstant' has been renamed to 'BoxForConstants.anonymousEnumConstant'}}
  _ = BoxForConstants.anonymousEnumConstant // okay
}

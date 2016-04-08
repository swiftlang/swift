// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -Xcc -w -parse -verify %s

// XFAIL: linux

import SwiftName

func test() {
  // Function name remapping
  drawString("hello", x: 3, y: 5)
  drawString("hello", 3, 5) // expected-error{{missing argument labels 'x:y:' in call}}

  // Enum name remapping.
  var color: ColorKind = CT_red
  var color2: ColorType = CT_Red // FIXME: should provide Fix-It expected-error{{use of undeclared type 'ColorType'}}

  // Enumerator remapping.
  var excuse: HomeworkExcuse = .dogAteIt
  excuse = .overslept // FIXME: should provide Fix-It  expected-error{{type 'HomeworkExcuse' has no member 'overslept'}}
  excuse = .tired
  excuse = .tooHard // FIXME: should provide Fix-It expected-error{{type 'HomeworkExcuse' has no member 'tooHard'}}
  excuse = .challenging

  // Typedef-of-anonymous-type-name renaming
  var p = Point()
  var p2 = PointType() // FIXME: should provide Fix-It expected-error{{use of unresolved identifier 'PointType'}}

  // Field name remapping
  p.x = 7

  // Typedef renaming
  var mi: MyInt = 5
  var mi2: my_int_t = 7 // FIXME: should provide Fix-It expected-error{{use of undeclared type 'my_int_t'}}
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse -verify %s

import MacrosRedefA
import MacrosRedefB

func testMacroRedef() {
  var s: String
  s = REDEF_1
  s = REDEF_2 // expected-error{{ambiguous use of 'REDEF_2'}}
}


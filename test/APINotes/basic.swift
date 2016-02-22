// RUN: %target-parse-verify-swift -I %S/Inputs/custom-modules
import APINotesTest

func testSwiftName() {
  moveTo(x: 0, y: 0, z: 0)
  moveTo(0, 0, 0) // expected-error{{missing argument labels 'x:y:z:' in call}}

  _ = global
  _ = ANTGlobalValue // expected-error{{use of unresolved identifier 'ANTGlobalValue'}}
}

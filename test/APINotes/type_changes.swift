// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks

// REQUIRES: objc_interop

import APINotesTest
import APINotesFrameworkTest


func testChangedTypes(tc: TypeChanges, a: A, i: Int) {
  _ = tc.method(with: i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'A?'}}
  let _: Int = tc.method(with: a) // expected-error{{cannot convert value of type 'A' to specified type 'Int'}}
}

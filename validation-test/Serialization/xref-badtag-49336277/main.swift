// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/itermod.swift -module-name itermod -emit-module -o %t/itermod.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/basemod.swift -module-name basemod -emit-module -o %t/basemod.swiftmodule -I %t
// RUN: %target-swift-frontend %S/Inputs/dependmod.swift -module-name dependmod -emit-module -o %t/dependmod.swiftmodule -I %t
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

import basemod
import dependmod

func test(b: MyBar) {
  b.callme()
  // Check that there was typechecking.
  b.non_existent() // expected-error {{no member 'non_existent'}}
}

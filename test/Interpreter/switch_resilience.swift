// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct -I%t -L%t -enable-library-evolution
// RUN: %target-codesign %t/%target-library-name(resilient_struct)
// RUN: %target-swiftc_driver -I %t -L %t %s -o %t/switch_resilience -lresilient_struct %target-rpath(%t)
// RUN: %target-codesign %t/switch_resilience
// RUN: %target-run %t/switch_resilience %t/%target-library-name(resilient_struct)

// REQUIRES: executable_test

import StdlibUnittest
import resilient_struct

var SwitchResilienceTestSuite = TestSuite("SwitchResilience")
defer { runAllTests() }

enum Enum {
case first(url: ResilientRef, void: Void)
}

func getEnum() -> Enum {
  let url = ResilientRef(r: Referent())
  return .first(url: url, void: ())
}
func getBool() -> Bool { return false }
func urlUser(_ u: ResilientRef) {}
func kraken() {}

SwitchResilienceTestSuite.test("Resilient Type Tuple Initialization") {
  switch getEnum() {
  case let .first(value) where getBool():
    urlUser(value.0)
  case .first:
    kraken()
  }
  kraken()
}

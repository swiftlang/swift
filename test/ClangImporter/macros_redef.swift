// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/macros_redef.h -typecheck %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/macros_redef.h -DCONFLICT -typecheck -verify %s

import MacrosRedefA
import MacrosRedefB

#if CONFLICT
import MacrosRedefWithSubmodules
import MacrosRedefWithSubmodules.TheSubmodule
import MacrosRedefWithParallelSubmodules.A
import MacrosRedefWithParallelSubmodules.B
#else
import MacrosRedefWithSubmodules.TheSubmodule
import MacrosRedefWithParallelSubmodules.A
#endif

func testFrameworkRedef() {
  var s: String
  s = REDEF_1
#if CONFLICT
  s = REDEF_2 // expected-error{{ambiguous use of 'REDEF_2'}}
#endif
}

func testBridgingHeaderRedef() {
  var s: String
  s = BRIDGING_HEADER_1
  s = BRIDGING_HEADER_2
  _ = s
}

func testSubmodules() {
  var s: String
  s = MRWS_REDEF_1
  s = MRWS_REDEF_2
  _ = s
}

func testParallelSubmodules() {
  var s: String
  s = MRWPS_REDEF_1
  s = MRWPS_REDEF_2 // expected-error{{ambiguous use of 'MRWPS_REDEF_2'}}
  _ = s
}


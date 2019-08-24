// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -enable-objc-interop -import-objc-header %S/Inputs/macros_redef.h -emit-silgen %s | %FileCheck -check-prefix=NEGATIVE %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -enable-objc-interop -import-objc-header %S/Inputs/macros_redef.h -DCONFLICT -typecheck -verify %s

// NEGATIVE-NOT: OLDTAG

import MacrosRedefA
import MacrosRedefB
import MacrosDeliberateRedefA
import MacrosDeliberateRedefB

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

func testDeliberateRedef() {
  var s: String
  s = MacrosDeliberateRedefA.MDR_REDEF_1
  s = MacrosDeliberateRedefB.MDR_REDEF_1
  s = MDR_REDEF_1

#if CONFLICT
  // The first two lines ought to work even when SILGen-ing, but the two
  // definitions of MDR_REDEF_2 end up getting the same mangled name.
  s = MacrosDeliberateRedefA.MDR_REDEF_2 // ok
  s = MacrosDeliberateRedefB.MDR_REDEF_2 // ok
  s = MDR_REDEF_2 // expected-error{{ambiguous use of 'MDR_REDEF_2'}}
#endif
}


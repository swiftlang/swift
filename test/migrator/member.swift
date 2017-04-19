// RUN: %target-swiftc_driver -update-code -F %S/mock-sdk -api-diff-data-file %S/API.json %s -o %t.member.result
// RUN: %FileCheck %s -check-prefix=SIMPLE_REPLACE < %t.member.result

import Bar

func foo(_ b: BarForwardDeclaredClass) -> Int32 {
  return barGlobalVariable
}

// SIMPLE_REPLACE: "text": "bar.memberVariable"
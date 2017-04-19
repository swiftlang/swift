// RUN: %target-swiftc_driver -update-code -F %S/mock-sdk -api-diff-data-file %S/API.json %s -o %t.member.result
// RUN: diff -u %S/Outputs/member.result %t.member.result

import Bar

func foo(_ b: BarForwardDeclaredClass) -> Int32 {
  return barGlobalVariable
}

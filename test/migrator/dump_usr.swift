// REQUIRES: objc_interop
// RUN: rm -rf %t && mkdir -p %t && %swift -update-code -primary-file %s -F %S/mock-sdk -api-diff-data-file %S/API.json -o %t/member.swift.remap -dump-usr | %FileCheck %s -check-prefix=USR

import Bar

func foo(_ b: BarForwardDeclaredClass) -> Int32 {
  return barGlobalVariable
}

// USR: c:objc(cs)BarForwardDeclaredClasss:s5Int32Vc:@barGlobalVariable

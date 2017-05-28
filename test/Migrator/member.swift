// REQUIRES: objc_interop
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -F %S/mock-sdk -api-diff-data-file %S/API.json -emit-migrated-file-path %t/member.swift.result -emit-remap-file-path %t/member.swift.remap -o /dev/null
// RUN: diff -u %S/member.swift.expected %t/member.swift.result

import Bar

func foo(_ b: BarForwardDeclaredClass, _ s: SomeItemSet) -> Int32 {
  let _ = s.theSimpleOldName
  return barGlobalVariable
}

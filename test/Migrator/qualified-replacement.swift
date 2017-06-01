// REQUIRES: objc_interop
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -F %S/mock-sdk -api-diff-data-file %S/qualified.json -emit-migrated-file-path %t/qualified-replacement.swift.result -emit-remap-file-path %t/qualified-replacement.swift.remap -o /dev/null
// RUN: diff -u %S/qualified-replacement.swift.expected %t/qualified-replacement.swift.result

import Bar
func foo() {
  _ = PropertyUserInterface.fieldPlus
  PropertyUserInterface.methodPlus(1)
  _ = FooComparisonResult.orderedSame
  let _ : FooComparisonResult = .orderedSame
}
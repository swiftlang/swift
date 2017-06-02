// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 3 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-3 %s

// REQUIRES: objc_interop

import APINotesFrameworkTest

func testRenamedClassMembers(obj: AnyObject) {
  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.doImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  obj.swift3DoImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3DoImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalDoImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.finalDoImportantThings()
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'importantInstanceProperty' has been renamed to 'swift3InstanceProperty'
  _ = obj.importantInstanceProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'importantInstanceProperty' has been renamed to 'finalInstanceProperty'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = obj.swift3InstanceProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3InstanceProperty' has been renamed to 'finalInstanceProperty'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalInstanceProperty' has been renamed to 'swift3InstanceProperty'
  _ = obj.finalInstanceProperty
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}

// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 5 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-5 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s

// REQUIRES: objc_interop

import APINotesFrameworkTest

func testRenamedClassMembers(obj: AnyObject) {
  // CHECK-DIAGS-4: [[@LINE+1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'swift4DoImportantThings()'
  obj.doImportantThings()
  // CHECK-DIAGS-5: [[@LINE-1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  obj.swift4DoImportantThings()
  // CHECK-DIAGS-5: [[@LINE-1]]:{{[0-9]+}}: error: 'swift4DoImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-4: [[@LINE+1]]:{{[0-9]+}}: error: 'finalDoImportantThings()' has been renamed to 'swift4DoImportantThings()'
  obj.finalDoImportantThings()
  // CHECK-DIAGS-5-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-4: [[@LINE+1]]:{{[0-9]+}}: error: 'importantInstanceProperty' has been renamed to 'swift4InstanceProperty'
  _ = obj.importantInstanceProperty
  // CHECK-DIAGS-5: [[@LINE-1]]:{{[0-9]+}}: error: 'importantInstanceProperty' has been renamed to 'finalInstanceProperty'

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = obj.swift4InstanceProperty
  // CHECK-DIAGS-5: [[@LINE-1]]:{{[0-9]+}}: error: 'swift4InstanceProperty' has been renamed to 'finalInstanceProperty'

  // CHECK-DIAGS-4: [[@LINE+1]]:{{[0-9]+}}: error: 'finalInstanceProperty' has been renamed to 'swift4InstanceProperty'
  _ = obj.finalInstanceProperty
  // CHECK-DIAGS-5-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}

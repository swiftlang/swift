// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 %s

// CHECK-SWIFT-4: func jumpTo(x: Double, y: Double, z: Double)
// CHECK-SWIFT-3: func jumpTo(x: Double, y: Double, z: Double)

// CHECK-SWIFT-4: func accept(_ ptr: UnsafeMutablePointer<Double>)
// CHECK-SWIFT-3: func acceptPointer(_ ptr: UnsafeMutablePointer<Double>?)

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 3 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-3 %s

import APINotesFrameworkTest

func testRenamedTopLevel() {
  var value = 0.0

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]
  accept(&value)
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:3: error: 'accept' has been renamed to 'acceptPointer(_:)'
  // CHECK-DIAGS-3: note: 'accept' was introduced in Swift 4

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]
  acceptPointer(&value)
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:3: error: 'acceptPointer' has been renamed to 'accept(_:)'
  // CHECK-DIAGS-4: note: 'acceptPointer' was obsoleted in Swift 4

  acceptDoublePointer(&value)
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:3: error: 'acceptDoublePointer' has been renamed to
  // CHECK-DIAGS-4: 'accept(_:)'
  // CHECK-DIAGS-3: 'acceptPointer(_:)'
  // CHECK-DIAGS: note: 'acceptDoublePointer' was obsoleted in Swift 3
}

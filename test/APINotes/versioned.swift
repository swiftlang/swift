// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 %s

// CHECK-SWIFT-4: func jumpTo(x: Double, y: Double, z: Double)
// CHECK-SWIFT-3: func jumpTo(x: Double, y: Double, z: Double)

// CHECK-SWIFT-4: func accept(_ ptr: UnsafeMutablePointer<Double>)
// CHECK-SWIFT-3: func acceptPointer(_ ptr: UnsafeMutablePointer<Double>?)

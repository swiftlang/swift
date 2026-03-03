// RUN: %target-swift-ide-test -print-module -module-to-print=PassAsParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: func extractValueFromPtr(_ b: IntBox!) -> Int32
// CHECK: func extractValueFromRef(_ b: IntBox) -> Int32
// CHECK: func extractValueFromConstRef(_ b: IntBox) -> Int32
// CHECK: func extractValueFromRefToPtr(_ b: inout IntBox!) -> Int32
// CHECK: func extractValueFromRefToConstPtr(_ b: inout IntBox!) -> Int32
// CHECK: func extractValueFromConstRefToPtr(_ b: IntBox!) -> Int32
// CHECK: func extractValueFromConstRefToConstPtr(_ b: IntBox!) -> Int32

// CHECK: func initializeByPtr(_ value: Int32, _ b: UnsafeMutablePointer<IntBox?>!)

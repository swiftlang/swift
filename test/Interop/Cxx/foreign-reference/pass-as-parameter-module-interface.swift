// RUN: %target-swift-ide-test -print-module -module-to-print=PassAsParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: func extractValueFromPtr(_ b: IntBox!) -> CInt
// CHECK: func extractValueFromRef(_ b: IntBox) -> CInt
// CHECK: func extractValueFromConstRef(_ b: IntBox) -> CInt
// CHECK: func extractValueFromRefToPtr(_ b: inout IntBox!) -> CInt
// CHECK: func extractValueFromRefToConstPtr(_ b: inout IntBox!) -> CInt
// CHECK: func extractValueFromConstRefToPtr(_ b: IntBox!) -> CInt
// CHECK: func extractValueFromConstRefToConstPtr(_ b: IntBox!) -> CInt

// CHECK: func initializeByPtr(_ value: CInt, _ b: UnsafeMutablePointer<IntBox?>!)

// RUN: %target-swift-ide-test -print-module -module-to-print=ClassesSecondHeader -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s


// Check-next is not used here because a lot of stuff is pulled in from classes.h

// CHECK: enum ClassesNS1 {
// CHECK:   enum ClassesNS2 {
// CHECK:     struct DefinedInDefs {
// CHECK:       init()
// CHECK:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:     }
// CHECK:   }
// CHECK: }

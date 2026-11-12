// RUN: %target-swift-frontend -interpret -Onone %s | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_interpreter

// At -Onone, array literals are rewritten to call prespecializations that live
// in SwiftOnoneSupport, so immediate mode has to be able to load that library.
// (This used to fail on Windows because we looked for libswiftSwiftOnoneSupport.dll.)

// CHECK: hi
print("hi")

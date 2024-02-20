// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(FrameworkA)) %S/Inputs/FrameworkA.swift -module-name FrameworkA -emit-module -emit-module-path %t/FrameworkA.swiftmodule -enable-library-evolution -emit-module-interface -emit-module-interface-path %t/FrameworkA.swiftinterface
// RUN: %target-build-swift-dylib(%t/%target-library-name(FrameworkB)) %S/Inputs/FrameworkB.swift -module-name FrameworkB -emit-module -emit-module-path %t/FrameworkB.swiftmodule -enable-library-evolution -enable-testing -I %t -L %t -lFrameworkA %target-rpath(%t)
// Remove the swiftmodule file of FrameworkA such that the test case only has a resilient view of this framework.
// RUN: rm %t/FrameworkA.swiftmodule
// RUN: %target-build-swift -I %t -L %t -lFrameworkB %s -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-codesign %t/%target-library-name(FrameworkA) %t/%target-library-name(FrameworkB)
// RUN: %target-run %t/main %t/%target-library-name(FrameworkA) %t/%target-library-name(FrameworkB) | %FileCheck --check-prefix=EXEC-CHECK %s

// REQUIRES: executable_test
// WebAssembly does not have stable dynamic linking ABI yet
// UNSUPPORTED: CPU=wasm32

@testable import FrameworkB

func runThis() {
    var c = SubThing()
    c.printThis()
// EXEC-CHECK: x 1
// EXEC-CHECK: y 2
// EXEC-CHECK: z 3
    print("y \(c.y)")
// EXEC-CHECK: y 2
}

runThis()

// RUN: %target-swift-frontend -emit-ir -parse-as-library -parse-stdlib %s -module-name Swift -previous-module-installname-map-file %S/Inputs/backward_deploy_nominal.json | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -parse-stdlib %s -module-name Swift -previous-module-installname-map-file %S/Inputs/backward_deploy_nominal.json | %FileCheck %s --check-prefix=ALSO

// RUN: %target-swift-frontend -emit-ir -parse-as-library -parse-stdlib %s -module-name Compatibility59 -module-abi-name Swift | %FileCheck %s --check-prefix=CHECK-SHIM
// RUN: %target-swift-frontend -emit-ir -parse-as-library -parse-stdlib %s -module-name Compatibility59 -module-abi-name Swift | %FileCheck %s --check-prefix=ALSO-SHIM

// RUN: %target-swift-emit-module-interface(%t/Swift.swiftinterface) %s -parse-as-library -parse-stdlib -module-name Swift
// RUN: %FileCheck %s --check-prefix=CHECK-INTERFACE < %t/Swift.swiftinterface

// REQUIRES: OS=macosx

// Let's just pretend...
@available(macOS 13, *)
@_originallyDefinedIn(module: "Swift;Compatibility59", macOS 14)
public struct Fridge<Contents> {}

// Ultimately, both modules must export the same nominal type descriptor.
// CHECK-DAG: @"$ss6FridgeVMn" =
// CHECK-SHIM-DAG: @"$ss6FridgeVMn" =

// The home module also has linker directives.
// CHECK-DAG: @"\01$ld$previous$libswiftCompatibility59.dylib$$1$13.0$14.0$_$ss6FridgeVMn$" =
// ALSO-NOT: $ld$hide$

// The compatibility shim does not have any linker directives.
// ALSO-SHIM-NOT: $ld$hide$
// ALSO-SHIM-NOT: $ld$previous$

// There is no @_originallyDefinedIn in the swiftinterface:
// CHECK-INTERFACE-NOT: @_originallyDefinedIn
// CHECK-INTERFACE: @available(macOS 13, *)
// CHECK-INTERFACE-NEXT: public struct Fridge<Contents> {

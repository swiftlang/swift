// With the "implementation" code generation model, clients emit their own
// copies of almost everything, so the TBD file only lists '@export(interface)'
// declarations. Code that clients emit can refer to one if it's
// '@usableFromInline'.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck -check-prefix OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift
// CHECK-DAG: "_$e3Lib16uniqueDefinitionSiyF"
@usableFromInline @export(interface)
func uniqueDefinition() -> Int { 40 }

// CHECK-DAG: "_$e3Lib15publicInterfaceSiyF"
@export(interface)
public func publicInterface() -> Int { uniqueDefinition() + helper() }

// Clients emit their own copies of everything else.
// CHECK-NOT: helper
// CHECK-NOT: generic
func helper() -> Int { 1 }

public func generic<T>(_ t: T) -> Int {
  uniqueDefinition() + helper() + MemoryLayout<T>.size
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(generic(Int64(1)))
    print(publicInterface())
  }
}

// OUTPUT: 49
// OUTPUT-NEXT: 41

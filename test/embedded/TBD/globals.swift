// A global's once-token and once-initializer have the same uniqueness as its
// storage: clients emit their own copies of all three for a global with the
// "implementation" model. With the "interface" model, only the storage is
// strong, and the once-token and once-initializer stay internal.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Lib.ll
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-interface.tbd -tbd-install_name Lib -validate-tbd-against-ir=all

// Clients emit their own copies of the global, and still initialize it once.
// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift
@export(interface)
public func makeValue() -> Int {
  print("initializing")
  return 42
}

// LIBRARY-IR-DAG: @"$e3Lib10lazyGlobalSivp" = linkonce_odr global
// LIBRARY-IR-DAG: @"$e3Lib10lazyGlobal_Wz" = linkonce_odr global
// LIBRARY-IR-DAG: define linkonce_odr {{.*}}@"$e3Lib10lazyGlobal_WZ"(
public var lazyGlobal: Int = makeValue()

// LIBRARY-IR-DAG: @"$e3Lib15interfaceGlobalSivp" = global
// LIBRARY-IR-DAG: @"$e3Lib15interfaceGlobal_Wz" = internal global
// LIBRARY-IR-DAG: define internal {{.*}}@"$e3Lib15interfaceGlobal_WZ"(
@export(interface)
public var interfaceGlobal: Int = makeValue() &+ 1

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(lazyGlobal)
    lazyGlobal += 1
    print(lazyGlobal)
    print(interfaceGlobal)
  }
}

// CHECK: initializing
// CHECK-NEXT: 42
// CHECK-NEXT: 43
// CHECK-NEXT: initializing
// CHECK-NEXT: 43

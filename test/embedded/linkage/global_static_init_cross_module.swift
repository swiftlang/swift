// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Every combination of optimization levels, in each code generation model.
// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// Only the "interface" global is initialized statically after serialization.
// RUN: %empty-directory(%t/with-module)
// RUN: %empty-directory(%t/without-module)
// RUN: %target-swift-frontend -emit-ir -emit-module-path %t/Lib.swiftmodule -o %t/with-module/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -O
// RUN: %FileCheck -check-prefix DEFAULT-IR %s < %t/with-module/Lib.ll
// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %FileCheck -check-prefix IMPLEMENTATION-IR %s < %t/Lib.ll

// Whether a module is emitted doesn't affect the IR, apart from the module ID
// and source file name on the first two lines.
// RUN: %target-swift-frontend -emit-ir -o %t/without-module/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -O
// RUN: tail -n +3 %t/with-module/Lib.ll > %t/with-module.ll
// RUN: tail -n +3 %t/without-module/Lib.ll > %t/without-module.ll
// RUN: cmp %t/with-module.ll %t/without-module.ll

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- Lib.swift
func makeArray() -> [Int] { [1, 2, 3] }

// DEFAULT-IR-DAG: @"$e3Lib7lazyVarSaySiGvp" = {{.*}}global %TSa zeroinitializer
// IMPLEMENTATION-IR-DAG: @"$e3Lib7lazyVarSaySiGvp" = linkonce_odr {{.*}}global %TSa zeroinitializer
public var lazyVar: [Int] = makeArray()

// DEFAULT-IR-DAG: @"$e3Lib7lazyLetSaySiGvp" = {{.*}}global %TSa zeroinitializer
public let lazyLet: [Int] = makeArray()

// DEFAULT-IR-DAG: @"$e3Lib19interfaceLazyGlobalSaySiGvp" = {{.*}}global %TSa <{
@export(interface)
public var interfaceLazyGlobal: [Int] = makeArray()

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(lazyLet[2])
    lazyVar.append(4)
    print(lazyVar.count)
    print(interfaceLazyGlobal.count)
  }
}

// CHECK: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 3

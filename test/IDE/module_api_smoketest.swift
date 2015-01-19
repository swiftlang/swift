// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift-ide-test -generate-module-api-description -- -target %target-triple -module-cache-path %t %s | FileCheck %s

public struct Struct1 {}

// CHECK:      Name:            module_api_smoketest
// CHECK-NEXT: NestedDecls:
// CHECK-NEXT:  Structs:
// CHECK-NEXT:     - Name:            Struct1


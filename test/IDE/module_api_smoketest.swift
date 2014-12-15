// RUN: %swift-ide-test -generate-module-api-description -- %s | FileCheck %s

public struct Struct1 {}

// CHECK:      Name:            module_api_smoketest
// CHECK-NEXT: NestedDecls:
// CHECK-NEXT:  Structs:
// CHECK-NEXT:     - Name:            Struct1


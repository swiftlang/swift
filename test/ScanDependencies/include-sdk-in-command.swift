// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -sdk %t/mysecretsdk.sdk

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

func foo() { print(1) }

// CHECK: "-sdk",
// CHECK-NEXT: mysecretsdk.sdk

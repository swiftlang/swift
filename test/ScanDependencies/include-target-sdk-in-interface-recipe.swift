// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface %s -o %t/deps.json -target-sdk-version 15.0 -target-sdk-name macosx15.0.test_name

// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

func foo() { print(1) }

// CHECK: "-target-sdk-name",
// CHECK-NEXT: "macosx15.0.test_name"
// CHECK: "-target-sdk-version",
// CHECK-NEXT: "15.0"

// RUN: %target-swift-frontend -emit-supported-features %s | %FileCheck %s

// CHECK: "SupportedArguments"
// CHECK: "abi"
// CHECK: "emit-module"
// CHECK: "LastOption"
// CHECK: "SupportedFeatures"
// CHECK: "LastFeature"

// RUN: %target-swift-frontend -emit-supported-features %s | %FileCheck %s

// CHECK: "SupportedArguments"
// CHECK: "emit-module"
// CHECK: "LastOption"
// CHECK: "SupportedFeatures"
// CHECK: "LastFeature"

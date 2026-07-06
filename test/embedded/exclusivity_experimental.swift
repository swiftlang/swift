// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -typecheck -verify -enforce-exclusivity=checked 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: warning: '-enforce-exclusivity=checked' is ignored in Embedded Swift unless also enabled by '-enable-experimental-feature EmbeddedDynamicExclusivity'


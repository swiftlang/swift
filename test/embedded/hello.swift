// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

print("Hello, Embedded Swift!")

// CHECK: Hello, Embedded Swift!

// Check that the "-enable-embedded-swift" is stored as a flag in the 
// Compile Unit.

// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -emit-ir -enable-experimental-feature Embedded -g %s -o - | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// CHECK:          !DICompileUnit({{.*}}flags: "-enable-embedded-swift"

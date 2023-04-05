// Check that the "-enable-experimental-cxx-interop" is stored as a flag in the 
// Compile Unit.

// RUN: %target-swift-frontend -emit-ir -enable-experimental-cxx-interop -g %s -o - | %FileCheck %s
// CHECK:          !DICompileUnit({{.*}}flags: "-enable-experimental-cxx-interop"

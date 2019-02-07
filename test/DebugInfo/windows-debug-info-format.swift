// RUN: %swift-frontend -target x86_64-unknown-windows-msvc -g -emit-ir %s \
// RUN:   | %FileCheck %s
// CHECK: i32 2, !"CodeView", i32 1}

// REQUIRES: OS=windows


// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -sdk /SWIFT_SYSROOT/MacOSX.sdk | %FileCheck %s
// Test that sysroot and SDK are stored in the debug info.
// CHECK: distinct !DICompileUnit({{.*}}sysroot: "/SWIFT_SYSROOT/MacOSX.sdk",
// LLDB-SAME:                          sdk: "MacOSX.sdk"

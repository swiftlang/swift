// Ensure that raw module ABI names are mangled correctly and without crashing
// IRGenDebugInfo.
//
// RUN: %target-swift-frontend -module-name foo_bar -module-abi-name foo/bar -g %s -emit-ir | %FileCheck %s

// CHECK: @"$s0016foobar_dsJCadlhaMXM"
// CHECK: @"$s0016foobar_dsJCadlha0020MyStruct_kiaBDEDGcjaVMn"
struct `My Struct` {}

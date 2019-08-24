// Check that the apple-names section is emitted on Darwin.
// Adapted from llvm/test/DebugInfo/X86/debugger-tune.ll
// RUN: %target-swiftc_driver -emit-object -g %s -o %t
// RUN: llvm-readobj -sections %t | %FileCheck --check-prefix=CHECK-LLDB %s

// CHECK-LLDB-NOT: debug_pubnames
// CHECK-LLDB:     apple_names
// CHECK-LLDB-NOT: debug_pubnames

// REQUIRES: OS=macosx

// Note: RUN lines are copied from substitutions for %target-run-simple-swift.
// Explicit references to `-toolchain-stdlib-rpath` are omitted.
// Explicit `-no-toolchain-stdlib-rpath` flag is added.

// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: xcrun --toolchain $TOOLCHAINS --sdk %sdk %swiftc_driver_plain %mcp_opt -no-toolchain-stdlib-rpath %s -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: not --crash %t/a.out 2>&1 | %FileCheck %s

// TF-797: test `-no-toolchain-stdlib-rpath` on Darwin platforms.
// Expected crash due to linker error because RPATH is `/usr/lib/swift` instead
// of toolchain standard library.

import Python
print(Python.import)

// CHECK: dyld: Library not loaded: @rpath/libswiftPython.dylib
// CHECK: Referenced from: BUILD_DIR/test-macosx-x86_64/Python/Output/toolchain_stdlib_rpath_false.swift.tmp/a.out
// CHECK: Reason: image not found

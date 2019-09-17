// Note: RUN lines are copied from substitutions for %target-run-simple-swift.
// Explicit references to `-toolchain-stdlib-rpath` are omitted.

// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: xcrun --toolchain $TOOLCHAINS --sdk %sdk %swiftc_driver_plain %mcp_opt %s -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: %t/a.out

// TF-797: test default true behavior for `-toolchain-stdlib-rpath` on Darwin platforms.
// Verify no linker issues without explicitly specifying `-toolchain-stdlib-rpath`.

import Python
print(Python.import)

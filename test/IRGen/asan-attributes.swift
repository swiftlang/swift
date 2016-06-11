// This test verifies that we add the function attributes used by ASan.

// RUN: not %target-swift-frontend -emit-ir -sanitize=address %s 2>&1 | FileCheck %s -check-prefix=ASAN

// XFAIL: linux

func test() {
}

// ASAN: argument '-sanitize=address' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.

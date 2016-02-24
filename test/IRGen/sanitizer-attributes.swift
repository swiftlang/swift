// This test verifies that we add the function attributes used by sanitizers.

// RUN: %target-swift-frontend -emit-ir -sanitize=address %s | FileCheck %s -check-prefix=ASAN

func test() {
}

// ASAN: Function Attrs: sanitize_address

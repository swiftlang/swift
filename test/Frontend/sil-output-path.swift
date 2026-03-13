// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-object -sil-output-path %t/test.sil %s -o %t/test.o
// RUN: %FileCheck -input-file %t/test.sil %s --check-prefix=SIL-CHECK
// RUN: test -f %t/test.o

// Test that -sil-output-path produces SIL output alongside normal compilation

func testFunction() -> Int {
    return 42
}

// SIL-CHECK: sil hidden @$s4test0A8FunctionSiyF : $@convention(thin) () -> Int

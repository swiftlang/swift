// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-object -ir-output-path %t/test.ll %s -o %t/test.o
// RUN: %FileCheck -input-file %t/test.ll %s --check-prefix=IR-CHECK
// RUN: test -f %t/test.o

// Test that -ir-output-path produces LLVM IR output alongside normal compilation

func testFunction() -> Int {
    return 42
}

let _ = testFunction()

// IR-CHECK: @"$s4test0A8FunctionSiyF"

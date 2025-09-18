// RUN: %empty-directory(%t)
// RUN: echo '"%s": { "sil": "%t/test.sil", "llvm-ir": "%t/test.ll" }' > %t/output-file-map.json
// RUN: %target-swift-frontend -emit-object -supplementary-output-file-map %t/output-file-map.json %s -o %t/test.o -module-name test
// RUN: %FileCheck -input-file %t/test.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/test.ll %s --check-prefix=IR-CHECK
// RUN: test -f %t/test.o

// Test that SIL and IR files can be requested via output file map

func testFunction() -> Int {
    return 42
}

let _ = testFunction()

// SIL-CHECK: sil hidden @$s4test0A8FunctionSiyF : $@convention(thin) () -> Int
// IR-CHECK: @"$s4test0A8FunctionSiyF"
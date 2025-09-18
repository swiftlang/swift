// RUN: %empty-directory(%t)

// Test that SIL and IR files can be requested via output file map

// Test primary file compilation with both SIL and IR in file map
// RUN: echo '{"%/s": {"sil": "%/t/primary.sil", "llvm-ir": "%/t/primary.ll"}, "%/S/../Driver/Inputs/main.swift": {"sil": "%/t/main.sil", "llvm-ir": "%/t/main.ll"}}' > %t/multi-map.json
// RUN: %target-swift-frontend -emit-object -supplementary-output-file-map %t/multi-map.json -primary-file %/s %/S/../Driver/Inputs/main.swift -o %t/primary.o -module-name test
// RUN: test -f %t/primary.sil && test -f %t/primary.ll && test -f %t/primary.o
// RUN: test ! -f %t/main.sil && test ! -f %t/main.ll
// RUN: %FileCheck -input-file %t/primary.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/primary.ll %s --check-prefix=IR-CHECK

// Test switching primary files - same map, different primary file
// RUN: %target-swift-frontend -emit-object -supplementary-output-file-map %t/multi-map.json -primary-file %/S/../Driver/Inputs/main.swift %/s -o %t/main-primary.o -module-name test
// RUN: test -f %t/main.sil && test -f %t/main.ll && test -f %t/main-primary.o
// RUN: %FileCheck -input-file %t/main.sil %s --check-prefix=MAIN-SIL-CHECK
// RUN: %FileCheck -input-file %t/main.ll %s --check-prefix=MAIN-IR-CHECK

// Test partial file maps: SIL-only and IR-only in one test
// RUN: echo '{"%/s": {"sil": "%/t/partial.sil", "llvm-ir": "%/t/partial.ll"}}' > %t/partial-map.json
// RUN: %target-swift-frontend -emit-object -supplementary-output-file-map %t/partial-map.json %/s -o %t/partial.o -module-name test
// RUN: test -f %t/partial.sil && test -f %t/partial.ll && test -f %t/partial.o
// RUN: %FileCheck -input-file %t/partial.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/partial.ll %s --check-prefix=IR-CHECK

func testFunction() -> Int {
    return 42
}

func runTest() {
    _ = testFunction()
}

// Function expected by main.swift
func libraryFunction() {}

// For module-qualified access
struct ThisModule {
    static func libraryFunction() {}
}

// SIL-CHECK: sil hidden @$s4test0A8FunctionSiyF : $@convention(thin) () -> Int
// IR-CHECK: @"$s4test0A8FunctionSiyF"

// MAIN-SIL-CHECK: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32
// MAIN-IR-CHECK: define{{.*}} i32 @main(

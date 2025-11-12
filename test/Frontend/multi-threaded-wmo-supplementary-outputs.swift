// Test that frontend properly handles multiple supplementary output paths
// using command line options in multi-threaded WMO mode.

// RUN: %empty-directory(%t)

// RUN: echo 'public func functionA() -> Int { return 42 }' > %t/FileA.swift
// RUN: echo 'public func functionB() -> String { return "hello" }' > %t/FileB.swift

// RUN: %target-swift-frontend -c %t/FileA.swift %t/FileB.swift \
// RUN:   -wmo -num-threads 2 -O -module-name TestModule \
// RUN:   -save-optimization-record-path %t/FileA.opt.yaml \
// RUN:   -save-optimization-record-path %t/FileB.opt.yaml \
// RUN:   -ir-output-path %t/FileA.ll \
// RUN:   -ir-output-path %t/FileB.ll \
// RUN:   -sil-output-path %t/TestModule.sil \
// RUN:   -o %t/FileA.o -o %t/FileB.o

// RUN: ls %t/FileA.opt.yaml
// RUN: ls %t/FileB.opt.yaml

// RUN: ls %t/FileA.ll
// RUN: ls %t/FileB.ll

// RUN: ls %t/TestModule.sil

// RUN: ls %t/FileA.o
// RUN: ls %t/FileB.o

// RUN: grep -q "functionA" %t/FileA.ll
// RUN: grep -q "functionB" %t/FileB.ll

// In multi-threaded WMO, each source file should generate its own optimization record file
// RUN: grep -q "functionA" %t/FileA.opt.yaml
// RUN: grep -q "functionB" %t/FileB.opt.yaml

// Verify the SIL output contains both functions (whole module)
// RUN: grep -q "functionA" %t/TestModule.sil
// RUN: grep -q "functionB" %t/TestModule.sil

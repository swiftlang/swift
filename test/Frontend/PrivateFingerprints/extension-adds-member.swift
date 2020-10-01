
// Test per-type-body fingerprints using simple extensions
//
// If the parser is allowed to use a body fingerprint for an extension
// this test will fail because usesA.swift won't be recompiled for the
// last step.


// =============================================================================
// Without the fingerprints
// =============================================================================

// Establish status quo


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/extension-adds-member/* %t
// RUN: cp %t/definesAB{-before,}.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift


// RUN: cd %t && %target-swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json  >& %t/output1


// Change one type, but uses of all types get recompiled

// RUN: cp %t/definesAB{-after,}.swift

// Seeing weird failure on CI, so ensure that definesAB.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesAB.swift

// RUN: cd %t && %target-swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift  -module-name main -output-file-map ofm.json  >& %t/output2


// This test checks for the status quo; it would be OK to be more conservative

// RUN: %FileCheck -check-prefix=CHECK-RECOMPILED-WO %s < %t/output2
// CHECK-RECOMPILED-WO: {compile: definesAB.o <= definesAB.swift}
// CHECK-RECOMPILED-WO: {compile: usesA.o <= usesA.swift}
// CHECK-RECOMPILED-WO: {compile: usesB.o <= usesB.swift}

// RUN: %FileCheck -check-prefix=CHECK-NOT-RECOMPILED-WO %s < %t/output2
// CHECK-NOT-RECOMPILED-WO-NOT: {compile: main.o <= main.swift}


// =============================================================================
// With the fingerprints
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/extension-adds-member/* %t
// RUN: cp %t/definesAB{-before,}.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %target-swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json  >& %t/output3

// Change one type, only uses of that type get recompiled

// RUN: cp %t/definesAB{-after,}.swift

// Seeing weird failure on CI, so ensure that definesAB.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesAB.swift

// RUN: cd %t && %target-swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json  >& %t/output4

// RUN: %FileCheck -check-prefix=CHECK-RECOMPILED-W %s < %t/output4
// RUN: %FileCheck -check-prefix=CHECK-NOT-RECOMPILED-W %s < %t/output4

// CHECK-RECOMPILED-W: {compile: definesAB.o <= definesAB.swift}
// CHECK-RECOMPILED-W: {compile: usesA.o <= usesA.swift}


// CHECK-NOT-RECOMPILED-W-NOT: {compile: main.o <= main.swift}

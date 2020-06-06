
// Test per-type-body fingerprints: the protocol case.
//

// =============================================================================
// Without the fingerprints
// =============================================================================

// Establish status quo


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/protocol-fingerprint/* %t
// RUN: cp %t/definesAB{-before,}.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json >&output1

// only-run-for-debugging: cp %t/usesB.swiftdeps %t/usesB1.swiftdeps

// Change one type, but uses of all types get recompiled

// RUN: cp %t/definesAB{-after,}.swift

// Seeing weird failure on CI, so ensure that definesAB.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesAB.swift

// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift  -module-name main -output-file-map ofm.json >&output2

// Save for debugging:
// only-run-for-debugging: cp %t/usesB.swiftdeps %t/usesB1.swiftdeps

// RUN: %FileCheck -check-prefix=CHECK-MAINAB-RECOMPILED %s < %t/output2

// CHECK-MAINAB-RECOMPILED: Queuing (initial): {compile: definesAB.o <= definesAB.swift}
// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesA.o <= usesA.swift}
// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesB.o <= usesB.swift}

// =============================================================================
// With the fingerprints
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/protocol-fingerprint/* %t
// RUN: cp %t/definesAB{-before,}.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json >&output3

// only-run-for-debugging: cp %t/usesB.swiftdeps %t/usesB3.swiftdeps


// Change one type, only uses of that type get recompiled

// RUN: cp %t/definesAB{-after,}.swift

// Seeing weird failure on CI, so ensure that definesAB.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesAB.swift

// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesAB.swift usesA.swift usesB.swift -module-name main -output-file-map ofm.json >&output4

// only-run-for-debugging: cp %t/usesB.swiftdeps %t/usesB4.swiftdeps

// RUN: %FileCheck -check-prefix=CHECK-MAINAB-RECOMPILED %s < %t/output4


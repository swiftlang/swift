
// Test per-type-body fingerprints
//

// =============================================================================
// Without the fingerprints
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/type-fingerprint/{main,a}.swift %t
// RUN: cp %S/Inputs/type-fingerprint/ofm.json %t
// RUN: cp %S/Inputs/type-fingerprint/b0.swift %t/b.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output1

// only-run-for-debugging: cp %t/b.swiftdeps %t/b1.swiftdeps

// Change one type, but uses of all types get recompiled

// RUN: cp %S/Inputs/type-fingerprint/b1.swift %t/b.swift

// Seeing weird failure on CI, so ensure that b.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/b.swift

// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output2

// Save for debugging:
// only-run-for-debugging: cp %t/b.swiftdeps %t/b2.swiftdeps

// RUN: %FileCheck -check-prefix=CHECK-MAINAB-RECOMPILED %s < %t/output2

// CHECK-MAINAB-RECOMPILED: Queuing (initial): {compile: b.o <= b.swift}
// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: a.o <= a.swift}


// =============================================================================
// With the fingerprints
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/type-fingerprint/{main,a}.swift %t
// RUN: cp %S/Inputs/type-fingerprint/ofm.json %t
// RUN: cp %S/Inputs/type-fingerprint/b0.swift %t/b.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output3

// only-run-for-debugging: cp %t/b.swiftdeps %t/b3.swiftdeps

// Change one type, only uses of that type get recompiled

// RUN: cp %S/Inputs/type-fingerprint/b1.swift %t/b.swift

// Seeing weird failure on CI, so ensure that b.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/b.swift

// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output4

// only-run-for-debugging: cp %t/b.swiftdeps %t/b4.swiftdeps

// RUN: %FileCheck -check-prefix=CHECK-MAINB-RECOMPILED %s < %t/output4

// CHECK-MAINB-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: a.o <= a.swift}
// CHECK-MAINB-RECOMPILED: Queuing because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-MAINB-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: // CHECK-MAINB-RECOMPILED: a.o <= a.swift}


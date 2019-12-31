
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
// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output1

// Change one type, but uses of all types get recompiled

// RUN: cp %S/Inputs/type-fingerprint/b1.swift %t/b.swift
// RUN: cd %t && %swiftc_driver  -disable-type-fingerprints -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output2

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-MAINAB-RECOMPILED %s < %t/output2

// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-MAINAB-RECOMPILED:   type 'main.B1' in b.swift -> source file main.swift
// CHECK-MAINAB-RECOMPILED: Queuing because of dependencies discovered later: {compile: a.o <= a.swift}
// CHECK-MAINAB-RECOMPILED:   type 'main.B2' in b.swift -> source file a.swift


// =============================================================================
// Without the fingerprints
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/type-fingerprint/{main,a}.swift %t
// RUN: cp %S/Inputs/type-fingerprint/ofm.json %t
// RUN: cp %S/Inputs/type-fingerprint/b0.swift %t/b.swift
// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output3

// Change one type, only uses of that type get recompiled

// RUN: cp %S/Inputs/type-fingerprint/b1.swift %t/b.swift
// RUN: cd %t && %swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental ./main.swift ./a.swift ./b.swift -module-name main -output-file-map ofm.json >&output4

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-MAINB-RECOMPILED %s < %t/output4


// CHECK-MAINB-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: a.o <= a.swift}
// CHECK-MAINB-RECOMPILED-NOT:   type 'main.B2' in b.swift -> source file a.swift
// CHECK-MAINB-RECOMPILED: Queuing because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-MAINB-RECOMPILED:   type 'main.B1' in b.swift -> source file main.swift
// CHECK-MAINB-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: a.o <= a.swift}
// CHECK-MAINB-RECOMPILED-NOT:   type 'main.B2' in b.swift -> source file a.swift


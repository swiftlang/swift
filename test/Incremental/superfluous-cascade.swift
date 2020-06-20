// =============================================================================
// Without private dependencies
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/superfluous-cascade/* %t
// RUN: cp %t/definesPoint{-before,}.swift
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -disable-direct-intramodule-dependencies -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift -module-name main -output-file-map ofm.json >&output1

// Change one type - the cascading edge causes us to rebuild everything but main

// RUN: cp %t/definesPoint{-after,}.swift
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesPoint.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -disable-direct-intramodule-dependencies -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift  -module-name main -output-file-map ofm.json >&output2

// RUN: %FileCheck -check-prefix=CHECK-STATUS-QUO-RECOMPILED %s < %t/output2

// CHECK-STATUS-QUO-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesPoint.o <= usesPoint.swift}
// CHECK-STATUS-QUO-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesDisplay.o <= usesDisplay.swift}


// =============================================================================
// With private dependencies
// =============================================================================

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/superfluous-cascade/* %t
// RUN: cp %t/definesPoint{-before,}.swift
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift -module-name main -output-file-map ofm.json -enable-direct-intramodule-dependencies >&output3

// Change one type - now only the user of that type rebuilds

// RUN: cp %t/definesPoint{-after,}.swift
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesPoint.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift  -module-name main -output-file-map ofm.json -enable-direct-intramodule-dependencies >&output4

// RUN: %FileCheck -check-prefix=CHECK-PRIVATE-RECOMPILED %s --dump-input=always < %t/output4

// CHECK-PRIVATE-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesPoint.o <= usesPoint.swift}
// CHECK-PRIVATE-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: usesDisplay.o <= usesDisplay.swift}

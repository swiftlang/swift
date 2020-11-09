// Establish baseline

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/superfluous-cascade/* %t
// RUN: cp %t/definesPoint{-before,}.swift
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift -module-name main -output-file-map ofm.json >&output3

// Change one type - now only the user of that type rebuilds

// RUN: cp %t/definesPoint{-after,}.swift
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesPoint.swift

// RUN: cd %t && %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesPoint.swift usesPoint.swift usesDisplay.swift  -module-name main -output-file-map ofm.json >&output4

// RUN: %FileCheck -check-prefix=CHECK-RECOMPILED %s --dump-input=always < %t/output4

// CHECK-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesPoint.o <= usesPoint.swift}
// CHECK-RECOMPILED-NOT: Queuing because of dependencies discovered later: {compile: usesDisplay.o <= usesDisplay.swift}

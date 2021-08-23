// Establish baseline

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/cross-file-failure/* %t
// RUN: cp %t/definesA{-one,}.swift
// RUN: touch -t 200101010101 %t/*.swift
// RUN: cd %t

// RUN: %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesA.swift usesA.swift -module-name main -output-file-map ofm.json >&output-baseline

// Change one type and cause a syntax error. This should cause _both_ files to
// rebuild.

// RUN: cp %t/definesA{-two,}.swift
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesA.swift

// RUN: not %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesA.swift usesA.swift -module-name main -output-file-map ofm.json

// RUN: cp %t/definesA{-three,}.swift
// RUN: touch -t 200401010101 %t/definesA.swift

// RUN: not %target-swiftc_driver -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesA.swift usesA.swift -module-name main -output-file-map ofm.json >&output-incremental

// RUN: %FileCheck -check-prefix=CHECK-RECOMPILED %s --dump-input=always < %t/output-incremental

// CHECK-RECOMPILED: Queuing (initial): {compile: definesA.o <= definesA.swift}
// CHECK-RECOMPILED: Queuing because of dependencies discovered later: {compile: usesA.o <= usesA.swift}

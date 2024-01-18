// Test that moving an overloaded member from one extension to another forces a
// rebuild of existing users. We do not strictly need this behavior to occur in
// all cases, but it's good to be safe.

// Establish status quo

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/extension-changes-member/* %t
// RUN: cp %t/definesS{-before,}.swift

// Seeing weird failure on CI, so set the mod times
// RUN: touch -t 200101010101 %t/*.swift

// RUN: cd %t && %target-swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesS.swift -module-name main -output-file-map ofm.json  >& %t/output3

// Change one type, only uses of that type get recompiled

// RUN: cp %t/definesS{-after,}.swift

// Seeing weird failure on CI, so ensure that definesS.swift is newer
// RUN: touch -t 200201010101 %t/*
// RUN: touch -t 200101010101 %t/*.swift
// RUN: touch -t 200301010101 %t/definesS.swift

// RUN: cd %t && %target-swiftc_driver  -enable-batch-mode -j2 -incremental -driver-show-incremental main.swift definesS.swift -module-name main -output-file-map ofm.json  >& %t/output4

// RUN: %FileCheck -check-prefix=CHECK-RECOMPILED-W %s < %t/output4

// CHECK-RECOMPILED-W: {compile: definesS.o <= definesS.swift}
// CHECK-RECOMPILED-W: {compile: main.o <= main.swift}

// RUN: %empty-directory(%t)
// RUN: touch %t/file1.swift %t/file2.swift %t/file3.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %target-swiftc_driver -driver-skip-execution -c -emit-module -module-name main -driver-print-jobs %s -experimental-emit-module-separately %t/file1.swift %t/file2.swift %t/file3.swift %t/main.swift 2>^1 | %FileCheck -check-prefix NORMAL %s
// RUN: %target-swiftc_driver -driver-skip-execution -c -emit-module -module-name main -driver-print-jobs -incremental %s -experimental-emit-module-separately %t/file1.swift %t/file2.swift %t/file3.swift %t/main.swift 2>^1 | %FileCheck -check-prefix INCREMENTAL %s

// Just test that we eat this argument. Only the new driver knows what to do
// here. The legacy driver will just fall back to a merge-modules job as usual.
// NORMAL: swift
// NORMAL-NOT: -experimental-emit-module-separately

// INCREMENTAL: swift
// INCREMENTAL: -merge-modules
// INCREMENTAL-NOT: -experimental-emit-module-separately

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-nested-types.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-nested-types.swift

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s

// Switch the order of the files.
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-nested-types.swift -disable-serialization-nested-type-lookup-table
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-nested-types.swift -disable-serialization-nested-type-lookup-table

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t -print-stats 2>&1 | %FileCheck -check-prefix=DISABLED %s
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file.swiftmodule -o %t -print-stats 2>&1 | %FileCheck -check-prefix=DISABLED %s

// REQUIRES: asserts

// CHECK: 4 Serialization - # of nested types resolved without full lookup
// DISABLED: Statistics
// DISABLED-NOT: nested types resolved without full lookup

public func useTypes(
  _: Outer.Inner,
  _: Outer.InnerAlias,
  _: OuterClass.Inner,
  _: OuterClass.InnerAlias) {}

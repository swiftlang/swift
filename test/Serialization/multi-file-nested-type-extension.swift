// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-nested-types.swift %S/Inputs/multi-file-nested-types-extensions.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-nested-types.swift %S/Inputs/multi-file-nested-types-extensions.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-3.swiftmodule %s %S/Inputs/multi-file-nested-types.swift -primary-file %S/Inputs/multi-file-nested-types-extensions.swift

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule %t/multi-file-3.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file.swiftmodule %t/multi-file-3.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file-3.swiftmodule %t/multi-file.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s

// REQUIRES: asserts

// CHECK: Statistics
// CHECK: 1 Serialization - # of same-module nested types resolved without lookup

// Note the Optional here and below; this was once necessary to produce a crash.
// Without it, the type of the parameter is initialized "early" enough to not
// cause issues.
public func useTypes(_: Outer.Callback?) {}
extension Outer {
  public typealias Callback = (Outer.InnerFromExtension) -> Void

  public func useTypes(_: Outer.Callback?) {}
}

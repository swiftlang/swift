// RUN: %empty-directory(%t)

// RUN: %swiftc_driver -whole-module-optimization %S/../Inputs/empty.swift -### 2>&1 | %FileCheck -check-prefix WMO %s
// WMO-NOT: -primary-file
// RUN: %swiftc_driver -whole-module-optimization -no-whole-module-optimization %S/../Inputs/empty.swift -### 2>&1 | %FileCheck -check-prefix NO-WMO %s
// NO-WMO: -primary-file

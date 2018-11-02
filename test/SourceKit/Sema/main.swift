// RUN: %sourcekitd-test -req=sema %s -- %s %S/Inputs/empty.swift -module-name main | %FileCheck %s -check-prefix=NO_ERROR
// RUN: %sourcekitd-test -req=sema %s -- %s %S/Inputs/top_level.swift -module-name main | %FileCheck %s -check-prefix=NO_ERROR
// RUN: %sourcekitd-test -req=sema %S/Inputs/empty.swift -- %s %S/Inputs/empty.swift -module-name main | %FileCheck %s -check-prefix=NO_ERROR
// NO_ERROR-NOT: source.diagnostic.severity.error

// RUN: %sourcekitd-test -req=sema %S/Inputs/top_level.swift -- %s %S/Inputs/top_level.swift -module-name main | %FileCheck %s -check-prefix=TOP_LEVEL_ERROR
// TOP_LEVEL_ERROR: key.filepath: {{.*}}top_level.swift
// TOP_LEVEL_ERROR-NEXT: key.severity: source.diagnostic.severity.error,
// TOP_LEVEL_ERROR-NEXT: key.description: {{.*}}top level

print("hi") // Top-level code.

// RUN: %empty-directory(%t)

// RUN: touch %t/a.swiftmodule
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -emit-module -module-name foo %s %t/a.swiftmodule 2>&1 | %FileCheck %s -check-prefix=SWIFTMODULE-DEBUG-INPUT
// SWIFTMODULE-DEBUG-INPUT: 0: input, "{{.*}}actions-dsym.swift", swift
// SWIFTMODULE-DEBUG-INPUT: 1: compile, {0}, swiftmodule
// SWIFTMODULE-DEBUG-INPUT: 2: input, "{{.*}}a.swiftmodule", swift
// SWIFTMODULE-DEBUG-INPUT: 3: merge-module, {1, 2}, swiftmodule

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %s 2>&1 | %FileCheck %s -check-prefix=DEBUG
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone -g %s 2>&1 | %FileCheck %s -check-prefix=DEBUG
// DEBUG: 0: input, "{{.*}}actions-dsym.swift", swift
// DEBUG: 1: compile, {0}, object
// DEBUG: 2: merge-module, {1}, swiftmodule
// DEBUG: 3: link, {1, 2}, image
// DEBUG: 4: generate-dSYM, {3}, dSYM

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone %s 2>&1 | %FileCheck %s -check-prefix=BASIC
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -gnone %s 2>&1 | %FileCheck %s -check-prefix=BASIC
// BASIC: 0: input, "{{.*}}actions-dsym.swift", swift
// BASIC: 1: compile, {0}, object
// BASIC: 2: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=DEBUG,VERIFY-DEBUG-INFO
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone -g -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=DEBUG,VERIFY-DEBUG-INFO
// VERIFY-DEBUG-INFO: 5: verify-debug-info, {4}, none

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gdwarf-types -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=EXEC-AND-MODULE,VERIFY-DEBUG-DWARF
// EXEC-AND-MODULE: 0: input, "{{.*}}actions-dsym.swift", swift
// EXEC-AND-MODULE: 1: compile, {0}, object
// EXEC-AND-MODULE: 2: merge-module, {1}, swiftmodule
// EXEC-AND-MODULE: 3: link, {1}, image
// VERIFY-DEBUG-DWARF-TYPES: 4: generate-dSYM, {3}, dSYM
// VERIFY-DEBUG-DWARF-TYPES: 5: verify-debug-info, {4}, none

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gline-tables-only -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=BASIC,VERIFY-DEBUG-LINE-TABLES
// VERIFY-DEBUG-LINE-TABLES-ONLY: 3: generate-dSYM, {2}, dSYM
// VERIFY-DEBUG-LINE-TABLES-ONLY: 4: verify-debug-info, {3}, none

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=MISSING-DEBUG-OPTION
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -gnone -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=MISSING-DEBUG-OPTION
// MISSING-DEBUG-OPTION: warning: ignoring '-verify-debug-info'; no debug info is being generated
// MISSING-DEBUG-OPTION: 0: input, "{{.*}}actions-dsym.swift", swift
// MISSING-DEBUG-OPTION: 1: compile, {0}, object
// MISSING-DEBUG-OPTION: 2: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -c %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-OBJECT
// DEBUG-OBJECT: 0: input, "{{.*}}actions-dsym.swift", swift
// DEBUG-OBJECT: 1: compile, {0}, object
// DEBUG-OBJECT-NOT: merge-module

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-MODULE
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone -g -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-MODULE
// DEBUG-MODULE: 0: input, "{{.*}}actions-dsym.swift", swift
// DEBUG-MODULE: 1: compile, {0}, object
// DEBUG-MODULE: 2: merge-module, {1}, swiftmodule
// DEBUG-MODULE: 3: link, {1, 2}, image
// DEBUG-MODULE: 4: generate-dSYM, {3}, dSYM

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -gnone -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=EXEC-AND-MODULE
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g -gnone -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=EXEC-AND-MODULE

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions 2>&1 | %FileCheck %s -check-prefix=DEBUG-MULTI
// DEBUG-MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// DEBUG-MULTI: 1: compile, {0}, object
// DEBUG-MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// DEBUG-MULTI: 3: compile, {2}, object
// DEBUG-MULTI: 4: input, "{{.*}}actions-dsym.swift", swift
// DEBUG-MULTI: 5: compile, {4}, object
// DEBUG-MULTI: 6: merge-module, {1, 3, 5}, swiftmodule
// DEBUG-MULTI: 7: link, {1, 3, 5, 6}, image
// DEBUG-MULTI: 8: generate-dSYM, {7}, dSYM


// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %t/a.o %t/b.o -o main 2>&1 | %FileCheck %s -check-prefix=LINK-ONLY
// LINK-ONLY: 0: input, "{{.*}}/a.o", object
// LINK-ONLY: 1: input, "{{.*}}/b.o", object
// LINK-ONLY: 2: link, {0, 1}, image

// RUN: touch %t/a.swiftmodule %t/b.swiftmodule
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=DEBUG-LINK-ONLY
// DEBUG-LINK-ONLY: 0: input, "{{.*}}/a.o", object
// DEBUG-LINK-ONLY: 1: input, "{{.*}}/b.o", object
// DEBUG-LINK-ONLY: 2: input, "{{.*}}/a.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 3: input, "{{.*}}/b.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 4: link, {0, 1, 2, 3}, image
// DEBUG-LINK-ONLY: 5: generate-dSYM, {4}, dSYM

// RUN: touch %t/c.swift
// LINK-SWIFTMODULES: 0: input, "{{.*}}/c.swift", swift
// LINK-SWIFTMODULES: 1: compile, {0}, object
// LINK-SWIFTMODULES: 2: input, "{{.*}}/a.o", object
// LINK-SWIFTMODULES: 3: input, "{{.*}}/b.o", object
// LINK-SWIFTMODULES: 4: input, "{{.*}}/a.swiftmodule", swiftmodule
// LINK-SWIFTMODULES: 5: input, "{{.*}}/b.swiftmodule", swiftmodule
// LINK-SWIFTMODULES: 6: link, {1, 2, 3, 4, 5}, image

// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %t/c.swift %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=LINK-DEBUG-SWIFTMODULES
// LINK-DEBUG-SWIFTMODULES: 0: input, "{{.*}}/c.swift", swift
// LINK-DEBUG-SWIFTMODULES: 1: compile, {0}, object
// LINK-DEBUG-SWIFTMODULES: 2: input, "{{.*}}/a.o", object
// LINK-DEBUG-SWIFTMODULES: 3: input, "{{.*}}/b.o", object
// LINK-DEBUG-SWIFTMODULES: 4: input, "{{.*}}/a.swiftmodule", swiftmodule
// LINK-DEBUG-SWIFTMODULES: 5: input, "{{.*}}/b.swiftmodule", swiftmodule
// LINK-DEBUG-SWIFTMODULES: 6: merge-module, {1}, swiftmodule
// LINK-DEBUG-SWIFTMODULES: 7: link, {1, 2, 3, 4, 5, 6}, image

// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.9 -g %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions -force-single-frontend-invocation 2>&1 | %FileCheck %s -check-prefix=WHOLE-MODULE
// WHOLE-MODULE: 0: input, "{{.*}}Inputs/main.swift", swift
// WHOLE-MODULE: 1: input, "{{.*}}Inputs/empty.swift", swift
// WHOLE-MODULE: 2: input, "{{.*}}actions-dsym.swift", swift
// WHOLE-MODULE: 3: compile, {0, 1, 2}, object
// WHOLE-MODULE: 4: link, {3}, image
// WHOLE-MODULE: 5: generate-dSYM, {4}, dSYM

// RUN: %empty-directory(%t)

// XFAIL: freebsd, linux

// RUN: %swiftc_driver -driver-print-actions %s 2>&1 | %FileCheck %s -check-prefix=BASIC
// BASIC: 0: input, "{{.*}}actions.swift", swift
// BASIC: 1: compile, {0}, object
// BASIC: 2: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -c %s 2>&1 | %FileCheck %s -check-prefix=BASICC
// BASICC: 0: input, "{{.*}}actions.swift", swift
// BASICC: 1: compile, {0}, object

// RUN: %swiftc_driver -driver-print-actions -emit-sil %s 2>&1 | %FileCheck %s -check-prefix=BASICSIL
// BASICSIL: 0: input, "{{.*}}actions.swift", swift
// BASICSIL: 1: compile, {0}, sil

// RUN: %swiftc_driver -driver-print-actions -emit-silgen %s 2>&1 | %FileCheck %s -check-prefix=BASICSILGEN
// BASICSILGEN: 0: input, "{{.*}}actions.swift", swift
// BASICSILGEN: 1: compile, {0}, raw-sil

// RUN: %swiftc_driver -driver-print-actions -S %s 2>&1 | %FileCheck %s -check-prefix=BASICASM
// BASICASM: 0: input, "{{.*}}actions.swift", swift
// BASICASM: 1: compile, {0}, assembly

// RUN: %swiftc_driver -driver-print-actions -emit-module %s 2>&1 | %FileCheck %s -check-prefix=BASICMODULE
// BASICMODULE: 0: input, "{{.*}}actions.swift", swift
// BASICMODULE: 1: compile, {0}, swiftmodule
// BASICMODULE: 2: merge-module, {1}, swiftmodule

// RUN: touch %t/a.swiftmodule
// RUN: %swiftc_driver -driver-print-actions -emit-module -module-name foo %s %t/a.swiftmodule 2>&1 | %FileCheck %s -check-prefix=SWIFTMODULE-INPUT
// SWIFTMODULE-INPUT: 0: input, "{{.*}}actions.swift", swift
// SWIFTMODULE-INPUT: 1: compile, {0}, swiftmodule
// SWIFTMODULE-INPUT: 2: input, "{{.*}}a.swiftmodule", swift
// SWIFTMODULE-INPUT: 3: merge-module, {1, 2}, swiftmodule

// RUN: %swiftc_driver -driver-print-actions -g -emit-module -module-name foo %s %t/a.swiftmodule 2>&1 | %FileCheck %s -check-prefix=SWIFTMODULE-INPUT
// SWIFTMODULE-DEBUG-INPUT: 0: input, "{{.*}}actions.swift", swift
// SWIFTMODULE-DEBUG-INPUT: 1: compile, {0}, swiftmodule
// SWIFTMODULE-DEBUG-INPUT: 2: input, "{{.*}}a.swiftmodule", swift
// SWIFTMODULE-DEBUG-INPUT: 3: merge-module, {1, 2}, swiftmodule

// RUN: %swiftc_driver -driver-print-actions -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=EXEC-AND-MODULE
// EXEC-AND-MODULE: 0: input, "{{.*}}actions.swift", swift
// EXEC-AND-MODULE: 1: compile, {0}, object
// EXEC-AND-MODULE: 2: merge-module, {1}, swiftmodule
// EXEC-AND-MODULE: 3: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -g %s 2>&1 | %FileCheck %s -check-prefix=DEBUG
// RUN: %swiftc_driver -driver-print-actions -gnone -g %s 2>&1 | %FileCheck %s -check-prefix=DEBUG
// DEBUG: 0: input, "{{.*}}actions.swift", swift
// DEBUG: 1: compile, {0}, object
// DEBUG: 2: merge-module, {1}, swiftmodule
// DEBUG: 3: link, {1, 2}, image

// RUN: %swiftc_driver -driver-print-actions -gnone %s 2>&1 | %FileCheck %s -check-prefix=BASIC
// RUN: %swiftc_driver -driver-print-actions -g -gnone %s 2>&1 | %FileCheck %s -check-prefix=BASIC

// RUN: %swiftc_driver -driver-print-actions -g -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=DEBUG
// RUN: %swiftc_driver -driver-print-actions -gnone -g -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=DEBUG

// RUN: %swiftc_driver -driver-print-actions -gdwarf-types -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=EXEC-AND-MODULE,VERIFY-DEBUG-DWARF
// VERIFY-DEBUG-DWARF-TYPES: 0: verify-debug-info, {4}, none

// RUN: %swiftc_driver -driver-print-actions -gline-tables-only -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=BASIC,VERIFY-DEBUG-LINE-TABLES
// VERIFY-DEBUG-LINE-TABLES-ONLY: 0: verify-debug-info, {3}, none

// RUN: %swiftc_driver -driver-print-actions -gnone -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=MISSING-DEBUG-OPTION
// RUN: %swiftc_driver -driver-print-actions -g -gnone -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefixes=MISSING-DEBUG-OPTION
// MISSING-DEBUG-OPTION: warning: ignoring '-verify-debug-info'; no debug info is being generated
// MISSING-DEBUG-OPTION: 0: input, "{{.*}}actions.swift", swift
// MISSING-DEBUG-OPTION: 1: compile, {0}, object
// MISSING-DEBUG-OPTION: 2: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -g -c %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-OBJECT
// DEBUG-OBJECT: 0: input, "{{.*}}actions.swift", swift
// DEBUG-OBJECT: 1: compile, {0}, object
// DEBUG-OBJECT-NOT: merge-module

// RUN: %swiftc_driver -driver-print-actions -g -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-MODULE
// RUN: %swiftc_driver -driver-print-actions -gnone -g -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=DEBUG-MODULE
// DEBUG-MODULE: 0: input, "{{.*}}actions.swift", swift
// DEBUG-MODULE: 1: compile, {0}, object
// DEBUG-MODULE: 2: merge-module, {1}, swiftmodule
// DEBUG-MODULE: 3: link, {1, 2}, image

// RUN: %swiftc_driver -driver-print-actions -gnone -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=EXEC-AND-MODULE
// RUN: %swiftc_driver -driver-print-actions -g -gnone -emit-executable -emit-module %s 2>&1 | %FileCheck %s -check-prefix=EXEC-AND-MODULE

// RUN: %swiftc_driver -driver-print-actions %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions 2>&1 | %FileCheck %s -check-prefix=MULTI
// MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// MULTI: 1: compile, {0}, object
// MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// MULTI: 3: compile, {2}, object
// MULTI: 4: input, "{{.*}}actions.swift", swift
// MULTI: 5: compile, {4}, object
// MULTI: 6: link, {1, 3, 5}, image

// RUN: %swiftc_driver -driver-print-actions -g %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions 2>&1 | %FileCheck %s -check-prefix=DEBUG-MULTI
// DEBUG-MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// DEBUG-MULTI: 1: compile, {0}, object
// DEBUG-MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// DEBUG-MULTI: 3: compile, {2}, object
// DEBUG-MULTI: 4: input, "{{.*}}actions.swift", swift
// DEBUG-MULTI: 5: compile, {4}, object
// DEBUG-MULTI: 6: merge-module, {1, 3, 5}, swiftmodule
// DEBUG-MULTI: 7: link, {1, 3, 5, 6}, image


// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-actions %t/a.o %t/b.o -o main 2>&1 | %FileCheck %s -check-prefix=LINK-ONLY
// RUN: %swiftc_driver -driver-print-actions -g %t/a.o %t/b.o -o main 2>&1 | %FileCheck %s -check-prefix=LINK-ONLY
// LINK-ONLY: 0: input, "{{.*}}/a.o", object
// LINK-ONLY: 1: input, "{{.*}}/b.o", object
// LINK-ONLY: 2: link, {0, 1}, image

// RUN: touch %t/a.swiftmodule %t/b.swiftmodule
// RUN: %swiftc_driver -driver-print-actions -g %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=DEBUG-LINK-ONLY
// DEBUG-LINK-ONLY: 0: input, "{{.*}}/a.o", object
// DEBUG-LINK-ONLY: 1: input, "{{.*}}/b.o", object
// DEBUG-LINK-ONLY: 2: input, "{{.*}}/a.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 3: input, "{{.*}}/b.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 4: link, {0, 1, 2, 3}, image

// RUN: touch %t/c.swift
// RUN: %swiftc_driver -driver-print-actions %t/c.swift %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=LINK-SWIFTMODULES
// LINK-SWIFTMODULES: 0: input, "{{.*}}/c.swift", swift
// LINK-SWIFTMODULES: 1: compile, {0}, object
// LINK-SWIFTMODULES: 2: input, "{{.*}}/a.o", object
// LINK-SWIFTMODULES: 3: input, "{{.*}}/b.o", object
// LINK-SWIFTMODULES: 4: input, "{{.*}}/a.swiftmodule", swiftmodule
// LINK-SWIFTMODULES: 5: input, "{{.*}}/b.swiftmodule", swiftmodule
// LINK-SWIFTMODULES: 6: link, {1, 2, 3, 4, 5}, image

// RUN: %swiftc_driver -driver-print-actions -g %t/c.swift %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=LINK-DEBUG-SWIFTMODULES
// LINK-DEBUG-SWIFTMODULES: 0: input, "{{.*}}/c.swift", swift
// LINK-DEBUG-SWIFTMODULES: 1: compile, {0}, object
// LINK-DEBUG-SWIFTMODULES: 2: input, "{{.*}}/a.o", object
// LINK-DEBUG-SWIFTMODULES: 3: input, "{{.*}}/b.o", object
// LINK-DEBUG-SWIFTMODULES: 4: input, "{{.*}}/a.swiftmodule", swiftmodule
// LINK-DEBUG-SWIFTMODULES: 5: input, "{{.*}}/b.swiftmodule", swiftmodule
// LINK-DEBUG-SWIFTMODULES: 6: merge-module, {1}, swiftmodule
// LINK-DEBUG-SWIFTMODULES: 7: link, {1, 2, 3, 4, 5, 6}, image

// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-actions %t/a.o %s -o main 2>&1 | %FileCheck %s -check-prefix=COMPILE-PLUS-OBJECT
// COMPILE-PLUS-OBJECT: 0: input, "{{.*}}/a.o", object
// COMPILE-PLUS-OBJECT: 1: input, "{{.*}}actions.swift", swift
// COMPILE-PLUS-OBJECT: 2: compile, {1}, object
// COMPILE-PLUS-OBJECT: 3: link, {0, 2}, image


// RUN: %swiftc_driver -driver-print-actions %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions -force-single-frontend-invocation 2>&1 | %FileCheck %s -check-prefix=WHOLE-MODULE
// WHOLE-MODULE: 0: input, "{{.*}}Inputs/main.swift", swift
// WHOLE-MODULE: 1: input, "{{.*}}Inputs/empty.swift", swift
// WHOLE-MODULE: 2: input, "{{.*}}actions.swift", swift
// WHOLE-MODULE: 3: compile, {0, 1, 2}, object
// WHOLE-MODULE: 4: link, {3}, image

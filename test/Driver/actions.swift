// RUN: rm -rf %t
// RUN: mkdir -p %t

// XFAIL: linux

// RUN: %swiftc_driver -driver-print-actions %s 2>&1 | FileCheck %s -check-prefix=BASIC
// BASIC: 0: input, "{{.*}}actions.swift", swift
// BASIC: 1: compile, {0}, object
// BASIC: 2: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -c %s 2>&1 | FileCheck %s -check-prefix=BASICC
// BASICC: 0: input, "{{.*}}actions.swift", swift
// BASICC: 1: compile, {0}, object

// RUN: %swiftc_driver -driver-print-actions -emit-sil %s 2>&1 | FileCheck %s -check-prefix=BASICSIL
// BASICSIL: 0: input, "{{.*}}actions.swift", swift
// BASICSIL: 1: compile, {0}, sil

// RUN: %swiftc_driver -driver-print-actions -emit-silgen %s 2>&1 | FileCheck %s -check-prefix=BASICSILGEN
// BASICSILGEN: 0: input, "{{.*}}actions.swift", swift
// BASICSILGEN: 1: compile, {0}, raw-sil

// RUN: %swiftc_driver -driver-print-actions -S %s 2>&1 | FileCheck %s -check-prefix=BASICASM
// BASICASM: 0: input, "{{.*}}actions.swift", swift
// BASICASM: 1: compile, {0}, assembly

// RUN: %swiftc_driver -driver-print-actions -emit-module %s 2>&1 | FileCheck %s -check-prefix=BASICMODULE
// BASICMODULE: 0: input, "{{.*}}actions.swift", swift
// BASICMODULE: 1: compile, {0}, swiftmodule
// BASICMODULE: 2: merge-module, {1}, swiftmodule

// RUN: %swiftc_driver -driver-print-actions -emit-executable -emit-module %s 2>&1 | FileCheck %s -check-prefix=EXEC-AND-MODULE
// EXEC-AND-MODULE: 0: input, "{{.*}}actions.swift", swift
// EXEC-AND-MODULE: 1: compile, {0}, object
// EXEC-AND-MODULE: 2: merge-module, {1}, swiftmodule
// EXEC-AND-MODULE: 3: link, {1}, image

// RUN: %swiftc_driver -driver-print-actions -g %s 2>&1 | FileCheck %s -check-prefix=DEBUG
// RUN: %swiftc_driver -driver-print-actions -gnone -g %s 2>&1 | FileCheck %s -check-prefix=DEBUG
// DEBUG: 0: input, "{{.*}}actions.swift", swift
// DEBUG: 1: compile, {0}, object
// DEBUG: 2: merge-module, {1}, swiftmodule
// DEBUG: 3: link, {1, 2}, image
// DEBUG: 4: generate-dSYM, {3}, dSYM

// RUN: %swiftc_driver -driver-print-actions -gnone %s 2>&1 | FileCheck %s -check-prefix=BASIC
// RUN: %swiftc_driver -driver-print-actions -g -gnone %s 2>&1 | FileCheck %s -check-prefix=BASIC

// RUN: %swiftc_driver -driver-print-actions -g -c %s 2>&1 | FileCheck %s -check-prefix=DEBUG-OBJECT
// DEBUG-OBJECT: 0: input, "{{.*}}actions.swift", swift
// DEBUG-OBJECT: 1: compile, {0}, object
// DEBUG-OBJECT-NOT: merge-module

// RUN: %swiftc_driver -driver-print-actions -g -emit-executable -emit-module %s 2>&1 | FileCheck %s -check-prefix=DEBUG-MODULE
// RUN: %swiftc_driver -driver-print-actions -gnone -g -emit-executable -emit-module %s 2>&1 | FileCheck %s -check-prefix=DEBUG-MODULE
// DEBUG-MODULE: 0: input, "{{.*}}actions.swift", swift
// DEBUG-MODULE: 1: compile, {0}, object
// DEBUG-MODULE: 2: merge-module, {1}, swiftmodule
// DEBUG-MODULE: 3: link, {1, 2}, image
// DEBUG-MODULE: 4: generate-dSYM, {3}, dSYM

// RUN: %swiftc_driver -driver-print-actions -gnone -emit-executable -emit-module %s 2>&1 | FileCheck %s -check-prefix=EXEC-AND-MODULE
// RUN: %swiftc_driver -driver-print-actions -g -gnone -emit-executable -emit-module %s 2>&1 | FileCheck %s -check-prefix=EXEC-AND-MODULE

// RUN: %swiftc_driver -driver-print-actions %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions 2>&1 | FileCheck %s -check-prefix=MULTI
// MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// MULTI: 1: compile, {0}, object
// MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// MULTI: 3: compile, {2}, object
// MULTI: 4: input, "{{.*}}actions.swift", swift
// MULTI: 5: compile, {4}, object
// MULTI: 6: link, {1, 3, 5}, image

// RUN: %swiftc_driver -driver-print-actions -g %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions 2>&1 | FileCheck %s -check-prefix=DEBUG-MULTI
// DEBUG-MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// DEBUG-MULTI: 1: compile, {0}, object
// DEBUG-MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// DEBUG-MULTI: 3: compile, {2}, object
// DEBUG-MULTI: 4: input, "{{.*}}actions.swift", swift
// DEBUG-MULTI: 5: compile, {4}, object
// DEBUG-MULTI: 6: merge-module, {1, 3, 5}, swiftmodule
// DEBUG-MULTI: 7: link, {1, 3, 5, 6}, image
// DEBUG-MULTI: 8: generate-dSYM, {7}, dSYM


// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-actions %t/a.o %t/b.o -o main 2>&1 | FileCheck %s -check-prefix=LINK-ONLY
// LINK-ONLY: 0: input, "{{.*}}/a.o", object
// LINK-ONLY: 1: input, "{{.*}}/b.o", object
// LINK-ONLY: 2: link, {0, 1}, image

// RUN: touch %t/a.swiftmodule %t/b.swiftmodule
// RUN: %swiftc_driver -driver-print-actions -g %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | FileCheck %s -check-prefix=DEBUG-LINK-ONLY
// DEBUG-LINK-ONLY: 0: input, "{{.*}}/a.o", object
// DEBUG-LINK-ONLY: 1: input, "{{.*}}/b.o", object
// DEBUG-LINK-ONLY: 2: input, "{{.*}}/a.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 3: input, "{{.*}}/b.swiftmodule", swiftmodule
// DEBUG-LINK-ONLY: 4: merge-module, {0, 1, 2, 3}, swiftmodule
// DEBUG-LINK-ONLY: 5: link, {0, 1, 2, 3, 4}, image
// DEBUG-LINK-ONLY: 6: generate-dSYM, {5}, dSYM


// RUN: %swiftc_driver -driver-print-actions %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions -force-single-frontend-invocation 2>&1 | FileCheck %s -check-prefix=WHOLE-MODULE
// WHOLE-MODULE: 0: input, "{{.*}}Inputs/main.swift", swift
// WHOLE-MODULE: 1: input, "{{.*}}Inputs/empty.swift", swift
// WHOLE-MODULE: 2: input, "{{.*}}actions.swift", swift
// WHOLE-MODULE: 3: compile, {0, 1, 2}, object
// WHOLE-MODULE: 4: link, {3}, image

// RUN: %swiftc_driver -driver-print-actions -g %S/Inputs/main.swift %S/../Inputs/empty.swift %s -module-name actions -force-single-frontend-invocation 2>&1 | FileCheck %s -check-prefix=WHOLE-MODULE -check-prefix=WHOLE-MODULE-DEBUG
// WHOLE-MODULE-DEBUG: 5: generate-dSYM, {4}, dSYM

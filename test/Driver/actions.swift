// RUN: %swift_driver -driver-print-actions %s 2>&1 | FileCheck %s -check-prefix=BASIC
// BASIC: 0: input, "{{.*}}actions.swift", swift
// BASIC: 1: compile, {0}, object
// BASIC: 2: link, {1}, image

// RUN: %swift_driver -driver-print-actions -c %s 2>&1 | FileCheck %s -check-prefix=BASICC
// BASICC: 0: input, "{{.*}}actions.swift", swift
// BASICC: 1: compile, {0}, object

// RUN: %swift_driver -driver-print-actions -emit-sil %s 2>&1 | FileCheck %s -check-prefix=BASICSIL
// BASICSIL: 0: input, "{{.*}}actions.swift", swift
// BASICSIL: 1: compile, {0}, sil

// RUN: %swift_driver -driver-print-actions -emit-silgen %s 2>&1 | FileCheck %s -check-prefix=BASICSILGEN
// BASICSILGEN: 0: input, "{{.*}}actions.swift", swift
// BASICSILGEN: 1: compile, {0}, sil

// RUN: %swift_driver -driver-print-actions -S %s 2>&1 | FileCheck %s -check-prefix=BASICASM
// BASICASM: 0: input, "{{.*}}actions.swift", swift
// BASICASM: 1: compile, {0}, assembly

// RUN: %swift_driver -driver-print-actions %S/Inputs/main.swift %S/Inputs/empty.swift %s 2>&1 | FileCheck %s -check-prefix=MULTI
// MULTI: 0: input, "{{.*}}Inputs/main.swift", swift
// MULTI: 1: compile, {0}, object
// MULTI: 2: input, "{{.*}}Inputs/empty.swift", swift
// MULTI: 3: compile, {2}, object
// MULTI: 4: input, "{{.*}}actions.swift", swift
// MULTI: 5: compile, {4}, object
// MULTI: 6: link, {1, 3, 5}, image
// RUN: rm -rf %t && mkdir -p %t
// RUN: touch %t/empty
// RUN: touch %t/empty.swiftmodule
// RUN: touch %t/empty.o
// RUN: touch %t/empty.h
// RUN: touch %t/empty.swift

// ERROR: error: unexpected input file: {{.*}}empty

// COMPILE: 0: input
// COMPILE: 1: compile, {0}, object

// RUN: %swift_driver -driver-print-actions %t/empty 2>&1 | FileCheck -check-prefix=LINK %s
// RUN: %swift_driver -driver-print-actions %t/empty.swiftmodule 2>&1 | FileCheck -check-prefix=LINK %s
// RUN: %swift_driver -driver-print-actions %t/empty.o 2>&1 | FileCheck -check-prefix=LINK %s
// RUN: not %swift_driver -driver-print-actions %t/empty.h 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: %swift_driver -driver-print-actions %t/empty.swift 2>&1 | FileCheck -check-prefix=COMPILE %s

// LINK: 0: input
// LINK: 1: link, {0}, image

// RUN: not %swift_driver -driver-print-actions -emit-module %t/empty 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: %swift_driver -driver-print-actions -emit-module %t/empty.swiftmodule 2>&1 | FileCheck -check-prefix=MODULE %s
// RUN: not %swift_driver -driver-print-actions -emit-module %t/empty.o 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: not %swift_driver -driver-print-actions -emit-module %t/empty.h 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: %swift_driver -driver-print-actions %t/empty.swift 2>&1 | FileCheck -check-prefix=COMPILE %s

// MODULE: 0: input
// MODULE: 1: merge-module, {0}, swiftmodule

// RUN: not %swift_driver -driver-print-actions -parse %t/empty 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: not %swift_driver -driver-print-actions -parse %t/empty.swiftmodule 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: not %swift_driver -driver-print-actions -parse %t/empty.o 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: not %swift_driver -driver-print-actions -parse %t/empty.h 2>&1 | FileCheck -check-prefix=ERROR %s
// RUN: %swift_driver -driver-print-actions %t/empty.swift 2>&1 | FileCheck -check-prefix=COMPILE %s

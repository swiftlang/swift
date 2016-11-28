// RUN: rm -rf %t && mkdir -p %t
// RUN: touch %t/empty
// RUN: touch %t/empty.swiftmodule
// RUN: touch %t/empty.o
// RUN: touch %t/empty.h
// RUN: touch %t/empty.swift

// ERROR: error: unexpected input file: {{.*}}empty

// COMPILE: 0: input
// COMPILE: 1: compile, {0}, object

// RUN: %swiftc_driver -driver-print-actions %t/empty 2>&1 | %FileCheck -check-prefix=LINK-%target-object-format %s
// RUN: not %swiftc_driver -driver-print-actions %t/empty.swiftmodule 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: %swiftc_driver -driver-print-actions %t/empty.o 2>&1 | %FileCheck -check-prefix=LINK-%target-object-format %s
// RUN: not %swiftc_driver -driver-print-actions %t/empty.h 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: %swiftc_driver -driver-print-actions %t/empty.swift 2>&1 | %FileCheck -check-prefix=COMPILE %s

// LINK-macho: 0: input
// LINK-macho: 1: link, {0}, image

// LINK-elf: 0: input
// LINK-elf: 1: swift-autolink-extract, {0}, autolink
// LINK-elf: 2: link, {0, 1}, image

// RUN: not %swiftc_driver -driver-print-actions -emit-module %t/empty 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: %swiftc_driver -driver-print-actions -emit-module %t/empty.swiftmodule 2>&1 | %FileCheck -check-prefix=MODULE %s
// RUN: not %swiftc_driver -driver-print-actions -emit-module %t/empty.o 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: not %swiftc_driver -driver-print-actions -emit-module %t/empty.h 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: %swiftc_driver -driver-print-actions %t/empty.swift 2>&1 | %FileCheck -check-prefix=COMPILE %s

// MODULE: 0: input
// MODULE: 1: merge-module, {0}, swiftmodule

// RUN: not %swiftc_driver -driver-print-actions -typecheck %t/empty 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: not %swiftc_driver -driver-print-actions -typecheck %t/empty.swiftmodule 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: not %swiftc_driver -driver-print-actions -typecheck %t/empty.o 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: not %swiftc_driver -driver-print-actions -typecheck %t/empty.h 2>&1 | %FileCheck -check-prefix=ERROR %s
// RUN: %swiftc_driver -driver-print-actions %t/empty.swift 2>&1 | %FileCheck -check-prefix=COMPILE %s

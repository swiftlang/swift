// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/external-cascade/* %t

// REQUIRES: rdar70772320

//
// This test establishes a chain of modules that all depend on a set of
// bridging headers. This test ensures that changes to external dependencies -
// especially those that aren't directly imported - cause incremental rebuilds.
//
// |bridging-header.h| - Module C    Module B    Module A
//         ^             -------- -> -------- -> --------
//         |                |                        ^
//         |                |                        |
// |another-header.h|        ------------------------

//
// Set up a clean incremental build of all three modules
//

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -enable-experimental-cross-module-incremental-build -module-name C -I %t -output-file-map %t/C.json -working-directory %t -import-objc-header %t/bridging-header.h -Xfrontend -validate-tbd-against-ir=none -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -enable-experimental-cross-module-incremental-build -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -enable-experimental-cross-module-incremental-build -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

//
// Now change a header and ensure that the rebuild cascades outwards
//

// RUN: rm %t/another-header.h
// RUN: cp %S/Inputs/external-cascade/another-header.h %t/another-header.h
// RUN: touch %t/another-header.h
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -enable-experimental-cross-module-incremental-build -module-name C -I %t -output-file-map %t/C.json -working-directory %t -import-objc-header %t/bridging-header.h -Xfrontend -validate-tbd-against-ir=none -driver-show-incremental -driver-show-job-lifecycle C.swift 2>&1 | %FileCheck -check-prefix MODULE-C %s
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -enable-experimental-cross-module-incremental-build -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift 2>&1 | %FileCheck -check-prefix MODULE-B %s
// RUN: touch %t/B.swiftmodule
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -enable-experimental-cross-module-incremental-build -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift 2>&1 | %FileCheck -check-prefix MODULE-A %s

// MODULE-C: Job finished: {generate-pch: bridging-header-[[BRIDGING_HEADER:.*]].pch <= bridging-header.h}
// MODULE-C: Job finished: {compile: C.o <= C.swift bridging-header-[[BRIDGING_HEADER]].pch}
// MODULE-C: Job finished: {merge-module: C.swiftmodule <= C.o}

// MODULE-B: Queuing because of external dependencies: {compile: B.o <= B.swift}
// MODULE-B: Job finished: {compile: B.o <= B.swift}
// MODULE-B: Job finished: {merge-module: B.swiftmodule <= B.o}


// MODULE-A: Queuing because of external dependencies: {compile: A.o <= A.swift}
// MODULE-A: Job finished: {compile: A.o <= A.swift}
// MODULE-A: Job finished: {merge-module: A.swiftmodule <= A.o}

//
// And ensure that the null build really is null.
//

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -enable-experimental-cross-module-incremental-build -module-name C -I %t -output-file-map %t/C.json -working-directory %t -import-objc-header %t/bridging-header.h -Xfrontend -validate-tbd-against-ir=none -driver-show-incremental -driver-show-job-lifecycle C.swift 2>&1 | %FileCheck -check-prefix MODULE-C-NULL %s
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -enable-experimental-cross-module-incremental-build -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift 2>&1 | %FileCheck -check-prefix MODULE-B-NULL %s
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -enable-experimental-cross-module-incremental-build -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift 2>&1 | %FileCheck -check-prefix MODULE-A-NULL %s

// MODULE-C-NULL: Job finished: {generate-pch: bridging-header-[[BRIDGING_HEADER:.*]].pch <= bridging-header.h}
// MODULE-C-NULL: Job skipped: {compile: C.o <= C.swift bridging-header-[[BRIDGING_HEADER]].pch}
// MODULE-C-NULL: Job skipped: {merge-module: C.swiftmodule <= C.o}

// MODULE-B-NULL: Job skipped: {compile: B.o <= B.swift}
// MODULE-B-NULL: Job skipped: {merge-module: B.swiftmodule <= B.o}

// MODULE-A-NULL: Job skipped: {compile: A.o <= A.swift}
// MODULE-A-NULL: Job skipped: {merge-module: A.swiftmodule <= A.o}

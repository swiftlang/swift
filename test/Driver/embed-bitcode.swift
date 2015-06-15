// RUN: %target-swiftc_driver -driver-print-bindings -embed-bitcode %s 2>&1 | FileCheck -check-prefix=CHECK-%target-object-format %s
// CHECK-macho: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-macho: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-macho: "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "embed-bitcode"}

// CHECK-elf: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-elf: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-elf: "swift-autolink-extract", inputs: ["[[OBJECT]]"], output: {autolink: "[[AUTOLINK:.*\.autolink]]"}
// CHECK-elf: "linux::Linker", inputs: ["[[OBJECT]]", "[[AUTOLINK]]"], output: {image: "main"}

// RUN: %target-swiftc_driver -embed-bitcode %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-FRONT -check-prefix=CHECK-FRONT-%target-object-format
// CHECK-FRONT: -frontend
// CHECK-FRONT: -emit-bc
// CHECK-FRONT: -frontend
// CHECK-FRONT: -c
// CHECK-FRONT: -embed-bitcode{{ }}
// CHECK-FRONT: -disable-llvm-optzns
// CHECK-FRONT-macho: ld{{"? }}
// CHECK-FRONT-macho: -bitcode_bundle

// RUN: %target-swiftc_driver -embed-bitcode-marker %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-MARKER -check-prefix=CHECK-MARKER-%target-object-format
// CHECK-MARKER: -frontend
// CHECK-MARKER: -c
// CHECK-MARKER: -embed-bitcode-marker
// CHECK-MARKER-NOT: -frontend
// CHECK-MARKER-macho: ld{{"? }}
// CHECK-MARKER-macho: -bitcode_bundle

// RUN: %target-swiftc_driver -embed-bitcode -Xcc -DDEBUG -Xllvm -fake-llvm-option -c -emit-module %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-MODULE
// CHECK-MODULE: -frontend
// CHECK-MODULE: -emit-bc
// CHECK-MODULE-DAG: -Xcc -DDEBUG
// CHECK-MODULE-DAG: -Xllvm -fake-llvm-option
// CHECK-MODULE-DAG: -emit-module-path
// CHECK-MODULE: -frontend
// CHECK-MODULE-NOT: -Xcc
// CHECK-MODULE-NOT: -DDEBUG
// CHECK-MODULE-NOT: -fake-llvm-option
// CHECK-MODULE-NOT: -emit-module-path
// CHECK-MODULE: -frontend
// CHECK-MODULE: -emit-module

// RUN: %target-swiftc_driver -embed-bitcode -force-single-frontend-invocation %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-SINGLE
// CHECK-SINGLE: -frontend
// CHECK-SINGLE: -emit-bc
// CHECK-SINGLE: -frontend
// CHECK-SINGLE: -c
// CHECK-SINGLE: -embed-bitcode
// CHECK-SINGLE: -disable-llvm-optzns

// RUN: %target-swiftc_driver -embed-bitcode -c -parse-as-library -emit-module -force-single-frontend-invocation %s -parse-stdlib -module-name Swift 2>&1 -### | FileCheck %s -check-prefix=CHECK-LIB-WMO
// CHECK-LIB-WMO: -frontend
// CHECK-LIB-WMO: -emit-bc
// CHECK-LIB-WMO: -parse-stdlib
// CHECK-LIB-WMO: -frontend
// CHECK-LIB-WMO: -c
// CHECK-LIB-WMO: -parse-stdlib
// CHECK-LIB-WMO: -embed-bitcode
// CHECK-LIB-WMO: -disable-llvm-optzns

// RUN: %target-swiftc_driver -embed-bitcode -c -parse-as-library -emit-module %s %S/../Inputs/empty.swift -module-name ABC 2>&1 -### | FileCheck %s -check-prefix=CHECK-LIB
// CHECK-LIB: swift -frontend
// CHECK-LIB: -emit-bc
// CHECK-LIB: -primary-file
// CHECK-LIB: swift -frontend
// CHECK-LIB: -c
// CHECK-LIB: -embed-bitcode
// CHECK-LIB: -disable-llvm-optzns
// CHECK-LIB: swift -frontend
// CHECK-LIB: -emit-bc
// CHECK-LIB: -primary-file
// CHECK-LIB: swift -frontend
// CHECK-LIB: -c
// CHECK-LIB: -embed-bitcode
// CHECK-LIB: -disable-llvm-optzns
// CHECK-LIB: swift -frontend
// CHECK-LIB: -emit-module
// CHECK-LIB-NOT: swift -frontend

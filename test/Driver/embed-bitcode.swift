// RUN: %target-swiftc_driver -driver-print-bindings -embed-bitcode %s 2>&1 | FileCheck -check-prefix=CHECK-%target-object-format %s
// CHECK-macho: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-macho: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-macho: "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "embed-bitcode"}

// CHECK-elf: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-elf: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-elf: "swift-autolink-extract", inputs: ["[[OBJECT]]"], output: {autolink: "[[AUTOLINK:.*\.autolink]]"}
// CHECK-elf: "linux::Linker", inputs: ["[[OBJECT]]", "[[AUTOLINK]]"], output: {image: "main"}

// RUN: %target-swiftc_driver -embed-bitcode %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-FRONT
// CHECK-FRONT: -frontend
// CHECK-FRONT: -emit-bc
// CHECK-FRONT: -frontend
// CHECK-FRONT: -c
// CHECK-FRONT: -embed-bitcode
// CHECK-FRONT: -disable-llvm-optzns

// RUN: %target-swiftc_driver -embed-bitcode-marker %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-MARKER
// CHECK-MARKER: -frontend
// CHECK-MARKER: -c
// CHECK-MARKER: -embed-bitcode-marker
// CHECK-MARKER-NOT: -frontend

// RUN: %target-swiftc_driver -embed-bitcode -c -emit-module %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-MODULE
// CHECK-MODULE: -frontend
// CHECK-MODULE: -emit-bc
// CHECK-MODULE: -emit-module-path
// CHECK-MODULE: -frontend
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

// RUN: %target-swiftc_driver -embed-bitcode -c -parse-as-library -emit-module -force-single-frontend-invocation %s -parse-stdlib -module-name Swift 2>&1 -### | FileCheck %s -check-prefix=CHECK-LIB
// CHECK-LIB: -frontend
// CHECK-LIB: -emit-bc
// CHECK-LIB: -parse-stdlib
// CHECK-LIB: -frontend
// CHECK-LIB: -c
// CHECK-LIB: -parse-stdlib
// CHECK-LIB: -embed-bitcode
// CHECK-LIB: -disable-llvm-optzns

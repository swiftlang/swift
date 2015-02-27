// RUN: %swiftc_driver -driver-print-bindings -embed-bitcode %s 2>&1 | FileCheck -check-prefix=CHECK-%target-object-format %s
// CHECK-macho: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-macho: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-macho: "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "embed-bitcode"}

// CHECK-elf: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"}
// CHECK-elf: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK-elf: "swift-autolink-extract", inputs: ["[[OBJECT]]"], output: {autolink: "[[AUTOLINK:.*\.autolink]]"}
// CHECK-elf: "linux::Linker", inputs: ["[[OBJECT]]", "[[AUTOLINK]]"], output: {image: "main"}

// RUN: %swiftc_driver -embed-bitcode %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-FRONT
// CHECK-FRONT: -frontend
// CHECK-FRONT: -emit-bc
// CHECK-FRONT: -frontend
// CHECK-FRONT: -c
// CHECK-FRONT: -embed-bitcode
// CHECK-FRONT: -disable-llvm-optzns

// RUN: %swiftc_driver -embed-bitcode-marker %s 2>&1 -### | FileCheck %s -check-prefix=CHECK-MARKER
// CHECK-MARKER: -frontend
// CHECK-MARKER: -c
// CHECK-MARKER: -embed-bitcode-marker
// CHECK-MARKER-NOT: -frontend

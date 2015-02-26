// RUN: %swiftc_driver -driver-print-bindings -embed-bitcode %s 2>&1 | FileCheck %s
// CHECK: "swift", inputs: ["{{.*}}embed-bitcode.swift"], output: {llvm-bc: "[[BC:.*\.bc]]"} 
// CHECK: "swift", inputs: ["[[BC]]"], output: {object: "[[OBJECT:.*\.o]]"}
// CHECK: "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "embed-bitcode"}

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

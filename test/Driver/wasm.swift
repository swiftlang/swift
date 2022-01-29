// RUN: %swiftc_driver -driver-print-jobs -target wasm32-unknown-wasi -v %s 2>&1 | %FileCheck %s -check-prefix=CHECK-WASM

// CHECK-WASM: swift{{.*}} -frontend -c -primary-file {{.*}} -target wasm32-unknown-wasi -disable-objc-interop
// CHECK-WASM: clang{{.*}} -lswiftCore --target=wasm32-unknown-wasi -v {{.*}}-o

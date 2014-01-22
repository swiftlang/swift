// RUN: %swift_driver -driver-print-jobs -repl -sdk %S/../Inputs/clang-importer-sdk 2>&1 | FileCheck %s
// RUN: env SDKROOT=%S/../Inputs/clang-importer-sdk %swift_driver -driver-print-jobs -repl 2>&1 | FileCheck %s

// CHECK-NOT: warning: no such SDK: '{{.*}}/Inputs/clang-importer-sdk'
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk

// RUN: %swift_driver -driver-print-jobs -repl -sdk %S/../Inputs/nonexistent-sdk 2>&1 | FileCheck %s --check-prefix=SDKWARNING
// RUN: env SDKROOT=%S/../Inputs/nonexistent-sdk %swift_driver -driver-print-jobs -repl 2>&1 | FileCheck %s --check-prefix=SDKWARNING

// SDKWARNING: warning: no such SDK: '{{.*}}/Inputs/nonexistent-sdk'
// SDKWARNING: -sdk {{.*}}/Inputs/nonexistent-sdk

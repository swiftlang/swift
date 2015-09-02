// RUN: %swiftc_driver -driver-print-jobs -g -sdk %S/../Inputs/clang-importer-sdk %s 2>&1 | FileCheck %s
// RUN: env SDKROOT=%S/../Inputs/clang-importer-sdk %swiftc_driver_plain -g -driver-print-jobs %s 2>&1 | FileCheck %s

// CHECK-NOT: warning: no such SDK:
// CHECK: bin/swift
// CHECK: Driver/sdk.swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK-NEXT: bin/swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK: bin/{{.+}} {{.*}}.o{{[ "]}}
// CHECK: {{-syslibroot|--sysroot}} {{.*}}/Inputs/clang-importer-sdk

// RUN: %swift_driver -driver-print-jobs -repl -sdk %S/Inputs/nonexistent-sdk 2>&1 | FileCheck %s --check-prefix=SDKWARNING
// RUN: %swift_driver -driver-print-jobs -sdk %S/Inputs/nonexistent-sdk 2>&1 | FileCheck %s --check-prefix=SDKWARNING
// RUN: env SDKROOT=%S/Inputs/nonexistent-sdk %swift_driver_plain -driver-print-jobs -repl 2>&1 | FileCheck %s --check-prefix=SDKWARNING

// SDKWARNING: warning: no such SDK: '{{.*}}/Inputs/nonexistent-sdk'
// SDKWARNING: -sdk {{.*}}/Inputs/nonexistent-sdk

// RUN: %swiftc_driver -driver-print-jobs -parse -sdk %S/../Inputs/clang-importer-sdk -module-cache-path /path/to/cache %s 2>&1 | FileCheck %s --check-prefix=CACHE-PATH

// CACHE-PATH: -module-cache-path /path/to/cache

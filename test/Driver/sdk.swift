// RUN: %swift_driver -driver-print-jobs -g -sdk %S/../Inputs/clang-importer-sdk %s 2>&1 | FileCheck %s
// RUN: env SDKROOT=%S/../Inputs/clang-importer-sdk %swift_driver_plain -g -driver-print-jobs %s 2>&1 | FileCheck %s

// CHECK-NOT: warning: no such SDK:
// CHECK: bin/swift
// CHECK: sdk.swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK-NEXT: bin/swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK-NEXT: bin/ld {{.*}}.o
// CHECK: -syslibroot {{.*}}/Inputs/clang-importer-sdk

// RUN: %swift_driver -driver-print-jobs -repl -sdk %S/Inputs/nonexistent-sdk 2>&1 | FileCheck %s --check-prefix=SDKWARNING
// RUN: env SDKROOT=%S/Inputs/nonexistent-sdk %swift_driver_plain -driver-print-jobs -repl 2>&1 | FileCheck %s --check-prefix=SDKWARNING

// SDKWARNING: warning: no such SDK: '{{.*}}/Inputs/nonexistent-sdk'
// SDKWARNING: -sdk {{.*}}/Inputs/nonexistent-sdk

// RUN: %swift_driver -driver-print-jobs -parse -sdk %S/../Inputs/clang-importer-sdk -module-cache-path /path/to/cache %s 2>&1 | FileCheck %s --check-prefix=CACHE-PATH

// CACHE-PATH: /path/to/cache


// Test SDK detection for -i.
// RUN: rm -rf %t && mkdir -p %t/usr/bin/

// RUN: cp %S/Inputs/xcrun-bad.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -i %s -### | FileCheck -check-prefix=NOSDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -integrated-repl -### | FileCheck -check-prefix=NOSDK %s

// NOSDK-NOT: -sdk

// RUN: cp %S/Inputs/xcrun.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -i %s -### | FileCheck -check-prefix=XCRUN-SDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -integrated-repl -### | FileCheck -check-prefix=XCRUN-SDK %s

// XCRUN-SDK: -sdk /path/to/sdk/

// RUN: cp %S/Inputs/xcrun-empty.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -i %s -### | FileCheck -check-prefix=ROOT-SDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -integrated-repl -### | FileCheck -check-prefix=ROOT-SDK %s

// ROOT-SDK: -sdk /{{\s|$}}

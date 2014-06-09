// RUN: %swift_driver -driver-print-jobs -g -sdk %S/../Inputs/clang-importer-sdk %s 2>&1 | FileCheck %s
// RUN: env SDKROOT=%S/../Inputs/clang-importer-sdk %swift_driver -g -driver-print-jobs %s 2>&1 | FileCheck %s

// CHECK-NOT: warning: no such SDK:
// CHECK: bin/swift
// CHECK: sdk.swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK-NEXT: bin/swift
// CHECK: -sdk {{.*}}/Inputs/clang-importer-sdk
// CHECK-NEXT: bin/ld {{.*}}.o
// CHECK: -syslibroot {{.*}}/Inputs/clang-importer-sdk

// RUN: %swift_driver -driver-print-jobs -repl -sdk %S/Inputs/nonexistent-sdk 2>&1 | FileCheck %s --check-prefix=SDKWARNING
// RUN: env SDKROOT=%S/Inputs/nonexistent-sdk %swift_driver -driver-print-jobs -repl 2>&1 | FileCheck %s --check-prefix=SDKWARNING

// SDKWARNING: warning: no such SDK: '{{.*}}/Inputs/nonexistent-sdk'
// SDKWARNING: -sdk {{.*}}/Inputs/nonexistent-sdk

// RUN: %swift_driver -driver-print-jobs -parse -sdk %S/../Inputs/clang-importer-sdk \
// RUN:          -module-cache-path /path/to/cache %s 2>&1 | FileCheck %s --check-prefix=CACHE-PATH

// CACHE-PATH: /path/to/cache


// Test SDK detection for -i, first in a clean environment, then in one that
// looks like the Xcode installation environment. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: %swift_driver -i %s -### | FileCheck -check-prefix=NOSDK %s
// RUN: %swift_driver -integrated-repl -### | FileCheck -check-prefix=NOSDK %s

// NOSDK-NOT: -sdk

// RUN: rm -rf %t && mkdir -p %t/usr/bin/
// RUN: ln %swift_driver_plain %t/usr/bin/swift

// RUN: cp %S/Inputs/xcrun-bad.sh %t/usr/bin/xcrun
// RUN: %t/usr/bin/swift -i %s -### | FileCheck -check-prefix=NOSDK %s
// RUN: %t/usr/bin/swift -integrated-repl -### | FileCheck -check-prefix=NOSDK %s

// RUN: cp %S/Inputs/xcrun.sh %t/usr/bin/xcrun
// RUN: %t/usr/bin/swift -i %s -### | FileCheck -check-prefix=XCRUN-SDK %s
// RUN: %t/usr/bin/swift -integrated-repl -### | FileCheck -check-prefix=XCRUN-SDK %s

// RUN: mkdir -p %t/Toolchains/Test.xctoolchain/usr/bin/
// RUN: mv %t/usr/bin/swift %t/Toolchains/Test.xctoolchain/usr/bin/swift
// RUN: %t/Toolchains/Test.xctoolchain/usr/bin/swift -i %s -### | FileCheck -check-prefix=XCRUN-SDK %s

// XCRUN-SDK: -sdk
// XCRUN-SDK: /path/to/sdk/


// Clean up the test executable because hard links are expensive.
// RUN: rm -f %t/Toolchains/Test.xctoolchain/usr/bin/swift

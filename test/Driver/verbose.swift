// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -v %s 2>&1 | %FileCheck %s -check-prefix=VERBOSE_CLANG
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-msvc -v %s 2>&1 | %FileCheck %s -check-prefix=VERBOSE_CLANG

// VERBOSE_CLANG: clang{{.*}} -v -o

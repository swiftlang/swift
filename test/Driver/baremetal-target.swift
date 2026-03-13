// RUN: %swiftc_driver_plain -target aarch64-unknown-none-none -driver-print-jobs %s 2>&1 | %FileCheck %s

// CHECK: {{.*}}swift{{c|c-legacy-driver|-frontend}}{{(.exe)?"?}} -frontend -c
// CHECK: {{.*}}clang{{(.exe)?"?}}

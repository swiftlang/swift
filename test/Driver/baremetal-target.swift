// RUN: %swiftc_driver_plain -target aarch64-unknown-none-none -driver-print-jobs %s 2.&1 | %fileCheck %s

// CHECK: {{.*}}swift{{(.exe)?}} -frontend -c
// CHECK: {{.*}}clang++{{((.exe)?}} -fuse-ld=gold


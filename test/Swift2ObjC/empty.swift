// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift-ide-test %clang-importer-sdk -module-cache-path=%t/clang-module-cache -print-as-objc %s -source-filename %s > %t/empty.h
// RUN: FileCheck %s < %t/empty.h

// CHECK: @import swift;

// CHECK: @import ObjectiveC;
// CHECK-NEXT: #include <stdint.h>
// CHECK-NEXT: #include <stddef.h>
// CHECK-NEXT: #include <stdbool.h>
// CHECK-NEXT: #if defined(__has_include)
// CHECK-NEXT: # if __has_include(<uchar.h>)
// CHECK-NEXT: #  include <uchar.h>
// CHECK-NEXT: # endif
// CHECK-NEXT: #endif

// CHECK-NOT: {{.}}

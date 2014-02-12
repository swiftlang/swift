// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift-ide-test %clang-importer-sdk -module-cache-path %t/clang-module-cache -print-as-objc %s -source-filename %s > %t/empty.h
// RUN: FileCheck %s < %t/empty.h
// RUN: %check-in-clang %t/empty.h

// CHECK: @import swift;

// CHECK-NOT: {{.}}

// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %s
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck %s
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s
// CHECK-NOT: Record{{.*}}record-empty

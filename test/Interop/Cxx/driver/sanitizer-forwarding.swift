// REQUIRES: OS=macosx || OS=linux-gnu

// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=address -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-ASAN
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=thread -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-TSAN
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=undefined -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-UBSAN
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=memtag-stack -dump-clang-diagnostics -target arm64-apple-macosx10.9 2>&1 | %FileCheck %s -check-prefix CHECK-MEMTAG
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-NONE

// Targets where Clang does not support TSan must not get -fsanitize=thread
// forwarded, even though Swift's frontend accepts -sanitize=thread.
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=thread -dump-clang-diagnostics -target arm64-apple-ios16.0 2>&1 | %FileCheck %s -check-prefix CHECK-TSAN-IOS-DEVICE

// Sanitizers Clang does support on iOS device must still be forwarded.
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=address -dump-clang-diagnostics -target arm64-apple-ios16.0 2>&1 | %FileCheck %s -check-prefix CHECK-ASAN-IOS-DEVICE

// TSan is supported on the iOS simulator, so it must be forwarded there.
// RUN: %swift_frontend_plain -typecheck -parse-stdlib %s -sanitize=thread -dump-clang-diagnostics -target arm64-apple-ios16.0-simulator 2>&1 | %FileCheck %s -check-prefix CHECK-TSAN-IOS-SIM

// CHECK-ASAN: clang importer cc1 args: {{.*}} '-fsanitize=address'
// CHECK-TSAN: clang importer driver args: {{.*}} '-fsanitize=thread'
// CHECK-UBSAN: clang importer driver args: {{.*}} '-fsanitize=undefined'
// CHECK-MEMTAG: clang importer driver args: {{.*}} '-fsanitize=memtag-stack'
// CHECK-NONE-NOT: '-fsanitize=address'
// CHECK-NONE-NOT: '-fsanitize=thread'
// CHECK-NONE-NOT: '-fsanitize=undefined'
// CHECK-NONE-NOT: '-fsanitize=memtag-stack'
// CHECK-TSAN-IOS-DEVICE-NOT: '-fsanitize=thread'
// CHECK-ASAN-IOS-DEVICE: clang importer {{.*}}args: {{.*}} '-fsanitize=address'
// CHECK-TSAN-IOS-SIM: clang importer {{.*}}args: {{.*}} '-fsanitize=thread'

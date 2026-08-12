// Platforms whose libc hides modern interfaces behind feature macros get
// _GNU_SOURCE defined for the importer.
// RUN: %swift -target x86_64-unknown-linux-gnu -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-GNU-SOURCE
// RUN: %swift -target x86_64-swift-linux-musl -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-GNU-SOURCE
// RUN: %swift -target aarch64-unknown-linux-android28 -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-GNU-SOURCE

// Clang predefines _GNU_SOURCE for C++, so the two modes agree.
// RUN: %swift -target x86_64-unknown-linux-gnu -typecheck %s -parse-stdlib -cxx-interoperability-mode=default -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-GNU-SOURCE

// Platforms that don't use glibc shouldn't be affected.
// RUN: %swift -target x86_64-unknown-freebsd -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-NO-GNU-SOURCE

// CHECK-GNU-SOURCE: -D_GNU_SOURCE
// CHECK-NO-GNU-SOURCE-NOT: -D_GNU_SOURCE

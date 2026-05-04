// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/mysdk %t/mysysroot

// -sdk alone is forwarded as --sysroot=<sdk>.
// RUN: %swift -target x86_64-unknown-linux-gnu -sdk %t/mysdk -parse-stdlib -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck -check-prefix CHECK-SDK %s

// -sysroot alone is forwarded as --sysroot=<sysroot>.
// RUN: %swift -target x86_64-unknown-linux-gnu -sysroot %t/mysysroot -parse-stdlib -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck -check-prefix CHECK-SYSROOT %s

// when both -sdk and -sysroot are given, -sysroot takes precedence.
// RUN: %swift -target x86_64-unknown-linux-gnu -sdk %t/mysdk -sysroot %t/mysysroot -parse-stdlib -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck -check-prefix CHECK-BOTH %s

// CHECK-SDK: clang importer driver args:
// CHECK-SDK: '--sysroot={{.*}}{{/|\\}}mysdk'

// CHECK-SYSROOT: clang importer driver args:
// CHECK-SYSROOT: '--sysroot={{.*}}{{/|\\}}mysysroot'

// CHECK-BOTH: clang importer driver args:
// CHECK-BOTH: '--sysroot={{.*}}{{/|\\}}mysysroot'
// CHECK-BOTH-NOT: '--sysroot={{.*}}{{/|\\}}mysdk'

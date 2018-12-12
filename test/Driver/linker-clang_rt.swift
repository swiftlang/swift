// Make sure that the platform-appropriate clang_rt library (found relative to
// the compiler) is included when using Swift as a linker (with Apple targets).

// We use hard links to make sure the Swift driver really thinks it's been
// moved.

// RUN: rm -rf %t
// RUN: %empty-directory(%t/bin)
// RUN: %hardlink-or-copy(from: %swift_driver_plain, to: %t/bin/swiftc)
// RUN: %empty-directory(%t/lib/clang/darwin/)

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-NO-RUNTIME %s

// RUN: touch %t/lib/clang/darwin/libclang_rt.osx.a %t/lib/clang/darwin/libclang_rt.ios.a %t/lib/clang/darwin/libclang_rt.tvos.a %t/lib/clang/darwin/libclang_rt.watchos.a

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix MACOS %s

// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix IOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix IOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target armv7s-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix IOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix IOS %s

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-ios9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix TVOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-tvos9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix TVOS %s

// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix WATCHOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target armv7k-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix WATCHOS %s

// Clean up the test executable because hard links are expensive.
// RUN: rm -f %t/bin/swiftc

// CHECK: {{(bin/)?}}ld{{"? }}
// CHECK-NO-RUNTIME-NOT: libclang_rt
// CHECK-MACOS-SAME: {{[^ ]+/lib/clang/darwin/libclang_rt.osx.a}}
// CHECK-IOS-SAME: {{[^ ]+/lib/clang/darwin/libclang_rt.ios.a}}
// CHECK-TVOS-SAME: {{[^ ]+/lib/clang/darwin/libclang_rt.tvos.a}}
// CHECK-WATCHOS-SAME: {{[^ ]+/lib/clang/darwin/libclang_rt.watchos.a}}
// CHECK-SAME: -o {{[^ ]+}}

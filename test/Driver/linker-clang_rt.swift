// Make sure that the platform-appropriate clang_rt library (found relative to
// the compiler) is included when using Swift as a linker (with Apple targets).

// We use hard links to make sure the Swift driver really thinks it's been
// moved.

// RUN: rm -rf %t
// RUN: %empty-directory(%t/bin)
// RUN: %hardlink-or-copy(from: %swift_driver_plain, to: %t/bin/swiftc)
// RUN: %empty-directory(%t/lib/swift/clang/lib/darwin/)

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-NO-RUNTIME %s

// RUN: touch %t/lib/swift/clang/lib/darwin/libclang_rt.osx.a %t/lib/swift/clang/lib/darwin/libclang_rt.ios.a %t/lib/swift/clang/lib/darwin/libclang_rt.iossim.a %t/lib/swift/clang/lib/darwin/libclang_rt.tvos.a %t/lib/swift/clang/lib/darwin/libclang_rt.tvossim.a %t/lib/swift/clang/lib/darwin/libclang_rt.watchos.a %t/lib/swift/clang/lib/darwin/libclang_rt.watchossim.a

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-MACOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-ios13.0-macabi %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-MACCATALYST %s

// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-ios7-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-ios7-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target armv7s-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-ios7-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-ios7 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-IOS %s

// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-tvos9-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-TVOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target x86_64-apple-tvos9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-TVOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-tvos9-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-TVOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-tvos9 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-TVOS %s

// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-watchos2-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-WATCHOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target i386-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-WATCHOSSIM %s
// RUN: %t/bin/swiftc -driver-print-jobs -target armv7k-apple-watchos2 %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-WATCHOS %s
// RUN: %t/bin/swiftc -driver-print-jobs -target arm64-apple-watchos2-simulator %S/../Inputs/empty.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-WATCHOSSIM %s

// Clean up the test executable because hard links are expensive.
// RUN: rm -f %t/bin/swiftc

// CHECK: {{(bin/)?}}ld{{(.exe)?"? }}
// CHECK-NO-RUNTIME-NOT: libclang_rt
// CHECK-MACCATALYST-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.osx.a}}
// CHECK-MACOS-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.osx.a}}
// CHECK-IOS-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.ios.a}}
// CHECK-IOSSIM-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.iossim.a}}
// CHECK-TVOS-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.tvos.a}}
// CHECK-TVOSSIM-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.tvossim.a}}
// CHECK-WATCHOS-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.watchos.a}}
// CHECK-WATCHOSSIM-SAME: {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)clang(/|\\\\)lib(/|\\\\)darwin(/|\\\\)libclang_rt.watchossim.a}}
// CHECK-SAME: -o {{[^ ]+}}

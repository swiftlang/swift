// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: CODEGENERATOR=X86

// RUN: %swift -target i686-unknown-windows-msvc -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-X86
// RUN: %swift -target x86_64-unknown-windows-msvc -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-X64
// RUN: %swift -target thumbv7-unknown-windows-msvc -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-ARM
// RUN: %swift -target aarch64-unknown-windows-msvc -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-ARM64

// CHECK-X86: -D_X86_
// CHECK-X64: -D_AMD64_
// CHECK-ARM: -D_ARM_
// CHECK-ARM64: -D_ARM64_


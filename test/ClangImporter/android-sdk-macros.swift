// RUN: %swift -target aarch64-unknown-linux-android28 -typecheck %s -parse-stdlib -dump-clang-diagnostics 2>&1 | %FileCheck %s -check-prefix CHECK-WEAK-SYMBOLS

// CHECK-WEAK-SYMBOLS: -D__ANDROID_UNAVAILABLE_SYMBOLS_ARE_WEAK__


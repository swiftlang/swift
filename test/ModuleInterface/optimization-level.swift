// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Lib.swiftinterface %s -O
// RUN: %target-swift-frontend -compile-module-from-interface %t/Lib.swiftinterface -Xllvm -sil-print-pass-name -o /dev/null 2>&1 | %FileCheck --check-prefix OPT %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Lib.swiftinterface %s -Onone
// RUN: %target-swift-frontend -compile-module-from-interface %t/Lib.swiftinterface -Xllvm -sil-print-pass-name -o /dev/null 2>&1 | %FileCheck --check-prefix UNOPT %s

// This is a bit of an implementation detail, but we want to make sure
// optimization passes don't run when compiling a .swiftinterface that was
// generated with -Onone.

// OPT: EagerSpecializer
// UNOPT-NOT: EagerSpecializer
public func f() {}


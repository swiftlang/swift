// RUN: %target-swift-frontend -Xllvm -sil-print-before=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=BEFORE %s
// RUN: %target-swift-frontend -Xllvm -sil-print-after=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=AFTER %s
// RUN: %target-swift-frontend -Xllvm -sil-disable-pass=definite-init -Xllvm -sil-print-pass-name -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=DISABLE %s

// BEFORE: SIL function before Guaranteed Passes Definite Initialization for Diagnostics (definite-init) (#1)
// AFTER: SIL function after Guaranteed Passes Definite Initialization for Diagnostics (definite-init) (#1)
// DISABLE: (Disabled) Stage: Guaranteed Passes Pass: Definite Initialization for Diagnostics (definite-init)
func foo() {}

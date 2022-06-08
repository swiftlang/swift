// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPUNSPEC-DHUNSPEC %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-destroy-hoisting=false \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPUNSPEC-DHOFF %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-destroy-hoisting=true \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPUNSPEC-DHON %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=false \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPOFF-DHUNSPEC %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=false \
// RUN:     -enable-destroy-hoisting=false \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPOFF-DHOFF %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=false \
// RUN:     -enable-destroy-hoisting=true \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPOFF-DHON %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=true \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPON-DHUNSPEC %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=true \
// RUN:     -enable-destroy-hoisting=false \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPON-DHOFF %s

// RUN: %target-swift-frontend %s \
// RUN:     -O \
// RUN:     -enable-copy-propagation=true \
// RUN:     -enable-destroy-hoisting=true \
// RUN:     -Xllvm -sil-print-pass-name \
// RUN:     -emit-ir \
// RUN:     -o /dev/null \
// RUN: 2>&1 | %FileCheck -check-prefix CHECK-CPON-DHON %s

// CHECK-CPUNSPEC-DHUNSPEC: copy-propagation
// CHECK-CPUNSPEC-DHUNSPEC: ssa-destroy-hoisting

// CHECK-CPUNSPEC-DHOFF: copy-propagation
// CHECK-CPUNSPEC-DHOFF-NOT: ssa-destroy-hoisting

// CHECK-CPUNSPEC-DHON: copy-propagation
// CHECK-CPUNSPEC-DHON: ssa-destroy-hoisting

// CHECK-CPOFF-DHUNSPEC-NOT: copy-propagation
// CHECK-CPOFF-DHUNSPEC-NOT: ssa-destroy-hoisting

// CHECK-CPOFF-DHOFF-NOT: copy-propagation
// CHECK-CPOFF-DHOFF-NOT: ssa-destroy-hoisting

// CHECK-CPOFF-DHON-NOT: copy-propagation
// CHECK-CPOFF-DHON: ssa-destroy-hoisting

// CHECK-CPON-DHUNSPEC: copy-propagation
// CHECK-CPON-DHUNSPEC: ssa-destroy-hoisting

// CHECK-CPON-DHOFF: copy-propagation
// CHECK-CPON-DHOFF-NOT: ssa-destroy-hoisting

// CHECK-CPON-DHON: copy-propagation
// CHECK-CPON-DHON: ssa-destroy-hoisting

func foo() {}

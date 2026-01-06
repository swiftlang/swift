// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil -serialize-diagnostics -serialize-diagnostics-path %t/test.diag

// RUN: c-index-test -read-diagnostics %t/test.diag 2>&1 | %FileCheck --check-prefix CHECK-DIAG %t/src/test.h

//--- test.h
static char other = 42;
static const char static_const_char_that_is_not_const = 1 + other;
// CHECK-DIAG: [[@LINE-1]]:{{.*}}: error: initializer element is not a compile-time constant
// CHECK-DIAG: error: failed to import bridging header

//--- main.swift
func foo() {
  print(static_const_char_that_is_not_const)
}

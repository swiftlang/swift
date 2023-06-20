// RUN: %empty-directory(%t)
// RUN: %sourcekitd-test -req=index-to-store %s -index-store-path %t/idx -index-unit-output-path %t/indexstore.o -- -c -module-name indexstoremodule -file-prefix-map %S=REMAPPED_SRC_ROOT -file-prefix-map %t=REMAPPED_OUT_DIR %s
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

class Foo {
  let member: Int
  init(member: Int) {
    self.member = member
  }
}

// CHECK: indexstore.o-{{.*}}
// CHECK: module-name: indexstoremodule
// CHECK: main-path: REMAPPED_SRC_ROOT/indexstore.swift
// CHECK: out-file: REMAPPED_OUT_DIR/indexstore.o
// CHECK: is-debug: 1
// CHECK: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule

// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck %s
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s
// CHECK: DEPEND START
// CHECK: Record | user | {{.*}}record-dependency.swift | record-dependency.swift-
// CHECK: DEPEND END

// RUN: echo 'func bar() {}' > %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx2 -emit-module -module-name main -emit-module-path %t/main.swiftmodule %s %t/s2.swift -o %t/file.o -o %t/s2.o
// RUN: c-index-test core -print-unit %t/idx2 | %FileCheck %s -check-prefix=TWO_RECORDS
// TWO_RECORDS: file.o-
// TWO_RECORDS: DEPEND START
// TWO_RECORDS: Record | user | {{.*}}record-dependency.swift | record-dependency.swift-
// TWO_RECORDS: DEPEND END
// TWO_RECORDS: s2.o-
// TWO_RECORDS: DEPEND START
// TWO_RECORDS: Record | user | {{.*}}s2.swift | s2.swift-
// TWO_RECORDS: DEPEND END

func foo() {}

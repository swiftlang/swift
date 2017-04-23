// RUN: rm -rf %t && mkdir -p %t && %swift -update-code -primary-file %s -emit-migrated-file-path %t/migrated_null_migration.swift -o %t/null_migration.remap
// RUN: diff -u %s %t/migrated_null_migration.swift

// This file tests that, if all migration passes are no-op,
// there are no changes to the file.

protocol P {
  func foo()
}

class C : P {
  func foo() {}
}

struct S : P {
  func foo() {}
}

enum E : P {
  func foo() {}
}


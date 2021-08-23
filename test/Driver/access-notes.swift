// RUN: %swiftc_driver -emit-executable -o %t.exe %s -access-notes-path %/S/Inputs/missing.accessnotes -### 2>&1 | %FileCheck %s

// CHECK: -access-notes-path SOURCE_DIR/test/Driver/Inputs/missing.accessnotes

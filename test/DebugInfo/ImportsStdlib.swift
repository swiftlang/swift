// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name NotTheStdlib %s -I %t -g -o - > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.ll

// RUN: %target-swift-frontend -c -parse-stdlib -module-name NotTheStdlib %s -I %t -g -o %t.o
// RUN: %llvm-dwarfdump -a %t.o > %t.dump
// RUN: %FileCheck -check-prefix=DWARF %s < %t.dump
// RUN: %FileCheck -check-prefix=NEGATIVE-DWARF %s < %t.dump

// CHECK-DAG: ![[MODULE:[0-9]+]] = !DIModule({{.*}}, name: "NotTheStdlib", includePath: "{{.*}}test{{.*}}DebugInfo{{.*}}"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[THISFILE:[0-9]+]], entity: ![[MODULE]]
// CHECK-DAG: ![[THISFILE]] = !DIFile(filename: "ImportsStdlib.swift", directory: "{{.*}}test/DebugInfo")

// NEGATIVE-NOT: !DIFile(filename: "Swift.swiftmodule"
// NEGATIVE-NOT: !DIModule({{.*}}, name: "Swift"

// DWARF: .debug_info
// DWARF: DW_TAG_module
// DWARF:   DW_AT_name ("NotTheStdlib")
// DWARF:   DW_AT_LLVM_include_path

// DWARF: file_names{{.*}} ImportsStdlib.swift

// NEGATIVE-DWARF-NOT: DW_AT_name ("Swift")

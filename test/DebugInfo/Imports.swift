// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/basic.swiftmodule %S/basic.swift

// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -c -module-name Foo %s -I %t -g -o %t.o
// RUN: %llvm-dwarfdump -a %t.o | %FileCheck --check-prefix=DWARF %s

// CHECK-DAG: ![[FOOMODULE:[0-9]+]] = !DIModule({{.*}}, name: "Foo", includePath: "{{.*}}test{{.*}}DebugInfo{{.*}}"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[THISFILE:[0-9]+]], entity: ![[FOOMODULE]]
// CHECK-DAG: ![[THISFILE]] = !DIFile(filename: "Imports.swift", directory: "{{.*}}test/DebugInfo")
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = !DIFile(filename: "Swift.swiftmodule"
// CHECK-DAG: ![[SWIFTMODULE:[0-9]+]] = !DIModule({{.*}}, name: "Swift"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[THISFILE]], entity: ![[SWIFTMODULE]]
// CHECK-DAG: ![[BASICMODULE:[0-9]+]] = !DIModule({{.*}}, name: "basic"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[THISFILE]], entity: ![[BASICMODULE]]
import basic
import typealias Swift.Optional

func markUsed<T>(_ t: T) {}
markUsed(basic.foo(1, 2))

// DWARF: .debug_info
// DWARF: DW_TAG_module
// DWARF:   DW_AT_name ("Foo")
// DWARF:   DW_AT_LLVM_include_path
// DWARF: DW_TAG_module
// DWARF:   DW_AT_name ("Swift")
// DWARF:   DW_AT_LLVM_include_path
// DWARF: DW_TAG_module
// DWARF:   DW_AT_name ("basic")

// DWARF-NOT: "Swift.Optional"

// DWARF-DAG: file_names{{.*}}
// DWARF-NEXT: "Imports.swift"

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/basic.swiftmodule %S/basic.swift

// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - | FileCheck %s
// RUN: %target-swift-frontend -c -module-name Foo %s -I %t -g -o %t.o
// RUN: llvm-dwarfdump %t.o | FileCheck --check-prefix=DWARF %s

// CHECK-DAG: ![[FOOMODULE:[0-9]+]] = !MDModule(name: "Foo"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[THISFILE:[0-9]+]], entity: ![[FOOMODULE]]
// CHECK-DAG: ![[THISFILE]] = !DIFile(filename: "Imports.swift", directory: "{{.*}}test/DebugInfo")
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = !DIFile(filename: "Swift.swiftmodule"
// CHECK-DAG: ![[SWIFTMODULE:[0-9]+]] = !MDModule(name: "Swift"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[SWIFTFILE]], entity: ![[SWIFTMODULE]]
// CHECK-DAG: ![[BASICFILE:[0-9]+]] = !DIFile(filename: "basic.swiftmodule"
// CHECK-DAG: ![[BASICMODULE:[0-9]+]] = !MDModule(name: "basic"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[BASICFILE]], entity: ![[BASICMODULE]]
import basic
import typealias Swift.Optional

func markUsed<T>(t: T) {}
markUsed(basic.foo(1, 2))

// DWARF: .debug_info
// DWARF: DW_TAG_module
// DWARF-DAG: "Foo"
// DWARF-DAG: "Swift"
// DWARF-DAG: "basic"

// DWARF-NOT: "Swift.Optional"

// DWARF-DAG: file_names{{.*}} Imports.swift
// DWARF-DAG: file_names{{.*}} Swift.swiftmodule
// DWARF-DAG: file_names{{.*}} basic.swift

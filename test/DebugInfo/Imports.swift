// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-ir -module-name Foo %s -I %S -enable-source-import -g -o - | FileCheck %s
// RUN: %swift -c -module-name Foo %s -I %S -enable-source-import -g -o - | llvm-dwarfdump - | FileCheck --check-prefix=DWARF %s
// CHECK-DAG: ![[FOOMODULE:[0-9]+]] = {{.*}}[ DW_TAG_module ] [Foo]
// CHECK-DAG: metadata ![[THISFILE:[0-9]+]], metadata ![[FOOMODULE]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[THISFILE]] = metadata {{.*}}[ DW_TAG_file_type ] [{{.*}}test/DebugInfo/Imports.swift]
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = {{.*}}[ DW_TAG_file_type ]{{.*}}Swift.swiftmodule
// CHECK-DAG: ![[SWIFTMODULE:[0-9]+]] = {{.*}}[ DW_TAG_module ] [Swift]
// CHECK-DAG: metadata ![[SWIFTFILE]], metadata ![[SWIFTMODULE]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK-DAG: metadata ![[BASICFILE:[0-9]+]], metadata ![[BASICMODULE:[0-9]+]], i32 [[@LINE+3]]} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[BASICFILE]] = {{.*}}basic.swift
// CHECK-DAG: ![[BASICMODULE]] = {{.*}}[ DW_TAG_module ] [basic]
import basic

println(basic.foo(1, 2))

// DWARF: .debug_info
// DWARF: DW_TAG_module
// DWARF-NEXT: "Foo"
// DWARF: DW_TAG_module
// DWARF-NEXT: "Swift"
// DWARF: DW_TAG_module
// DWARF-NEXT: "basic"

// DWARF-DAG: file_names{{.*}} Imports.swift
// DWARF-DAG: file_names{{.*}} Swift.swiftmodule
// DWARF-DAG: file_names{{.*}} basic.swift

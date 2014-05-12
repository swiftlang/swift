// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-ir -module-name Foo %s -I %S -enable-source-import -g -o - | FileCheck %s
// RUN: %swift -c -module-name Foo %s -I %S -enable-source-import -g -o - | llvm-dwarfdump - | FileCheck --check-prefix=DWARF %s
// CHECK: ![[FOOMODULE:[0-9]+]] = {{.*}}[ DW_TAG_module ] [Foo]
// CHECK: metadata ![[THISFILE:[0-9]+]], metadata ![[FOOMODULE]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK: ![[THISFILE]] = metadata {{.*}}[ DW_TAG_file_type ] [{{.*}}test/DebugInfo/Imports.swift]
// CHECK: metadata ![[SWIFTFILE:[0-9]+]], metadata ![[SWIFTMODULE:[0-9]+]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK: ![[SWIFTFILE]] = {{.*}}Swift.swiftmodule
// CHECK: ![[SWIFTMODULE]] = {{.*}}[ DW_TAG_module ] [Swift]
// CHECK: metadata ![[BASICFILE:[0-9]+]], metadata ![[BASICMODULE:[0-9]+]], i32 [[@LINE+3]]} ; [ DW_TAG_imported_module ]
// CHECK: ![[BASICFILE]] = {{.*}}basic.swift
// CHECK: ![[BASICMODULE]] = {{.*}}[ DW_TAG_module ] [basic]
import basic

println(basic.foo(1, 2))

// DWARF: .debug_info
// DWARF: DW_TAG_module
// DWARF-NEXT: "Foo"
// DWARF: DW_TAG_module
// DWARF-NEXT: "Swift"
// DWARF: DW_TAG_module
// DWARF-NEXT: "basic"

// DWARF: file_names{{.*}} Imports.swift
// DWARF: file_names{{.*}} Swift.swiftmodule
// DWARF: file_names{{.*}} basic.swift

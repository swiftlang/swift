// A (no longer) basic test for debug info.
// --------------------------------------------------------------------
// Verify that we don't emit any debug info by default.
// RUN: %target-swift-frontend %s -emit-ir -o - | FileCheck %s --check-prefix NDEBUG
// NDEBUG-NOT: !dbg
// NDEBUG-NOT: DW_TAG
// --------------------------------------------------------------------
// Verify that we don't emit any debug info with -gnone.
// RUN: %target-swift-frontend %s -emit-ir -gnone -o - | FileCheck %s --check-prefix NDEBUG
// --------------------------------------------------------------------
// Verify that we don't emit any type info with -gline-tables-only.
// RUN: %target-swift-frontend %s -emit-ir -gline-tables-only -o - | FileCheck %s --check-prefix CHECK-LINETABLES
// CHECK: !dbg
// CHECK-LINETABLES-NOT: DW_TAG_{{.*}}variable
// CHECK-LINETABLES-NOT: DW_TAG_structure_type
// CHECK-LINETABLES-NOT: DW_TAG_basic_type
// --------------------------------------------------------------------
// Now check that we do generate line+scope info with -g.
// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o - -disable-sil-linking | FileCheck %s --check-prefix=CHECK-NOSIL
// --------------------------------------------------------------------
//
// CHECK: foo
// CHECK-DAG: ret{{.*}}, !dbg ![[RET:[0-9]+]]
// CHECK-DAG: ![[FOO:[0-9]+]] = !DISubprogram(name: "foo",{{.*}} line: [[@LINE+2]],{{.*}} type: ![[FOOTYPE:[0-9]+]]
public
func foo(var a: Int64, var _ b: Int64) -> Int64 {
     // CHECK-DAG: !DILexicalBlock(scope: ![[FOO]],{{.*}} line: [[@LINE-1]], column: 49)
     // CHECK-DAG: ![[ASCOPE:.*]] = !DILocation(line: [[@LINE-2]], column: 14, scope: ![[FOO]])
     // Check that a is the first and b is the second argument.
     // CHECK-DAG: store i64 %0, i64* [[AVAL:.*]], align 8
     // CHECK-DAG: store i64 %1, i64* [[BVAL:.*]], align 8
     // CHECK-DAG: [[AVAL]] = getelementptr inbounds {{.*}}, [[AMEM:.*]], i32 0, i32 0
     // CHECK-DAG: [[BVAL]] = getelementptr inbounds {{.*}}, [[BMEM:.*]], i32 0, i32 0
     // CHECK-DAG: call void @llvm.dbg.declare(metadata [[AMEM]], metadata ![[AARG:.*]], metadata !{{[0-9]+}}), !dbg ![[ASCOPE]]
     // CHECK-DAG: call void @llvm.dbg.declare(metadata [[BMEM]], metadata ![[BARG:.*]], metadata !{{[0-9]+}})
     // CHECK-DAG: ![[AARG]] = !DILocalVariable(tag: DW_TAG_arg_variable, name: "a"
     // CHECK-DAG: ![[BARG]] = !DILocalVariable(tag: DW_TAG_arg_variable, name: "b"
     if b != 0 {
       // CHECK-DAG: !DILexicalBlock({{.*}} line: [[@LINE-1]]
       // Transparent inlined multiply:
       // CHECK-DAG: smul{{.*}}, !dbg ![[MUL:[0-9]+]]
       // CHECK-DAG: [[MUL]] = !DILocation(line: [[@LINE+4]], column: 16,
       // Runtime call to multiply function:
       // CHECK-NOSIL: @_TZFSsoi1mFTVSs5Int64S__S_{{.*}}, !dbg ![[MUL:[0-9]+]]
       // CHECK-NOSIL: [[MUL]] = !DILocation(line: [[@LINE+1]], column: 16,
       return a*b
     } else {
       // CHECK-DAG: ![[PARENT:[0-9]+]] = distinct !DILexicalBlock({{.*}} line: [[@LINE-1]], column: 13)
       var c: Int64 = 42
       // CHECK-DAG: ![[CONDITION:[0-9]+]] = distinct !DILexicalBlock(scope: ![[PARENT]], {{.*}}, line: [[@LINE+1]],
       if a == 0 {
         // CHECK-DAG: !DILexicalBlock(scope: ![[CONDITION]], {{.*}}, line: [[@LINE-1]], column: 18)
         // What about a nested scope?
         return 0
       }
       return c
     }
}

// CHECK-DAG: ![[MAINFILE:[0-9]+]] = !DIFile(filename: "basic.swift", directory: "{{.*}}DebugInfo")
// CHECK-DAG: !DICompileUnit({{.*}}file: ![[MAINFILE]],{{.*}} producer: "{{.*}}Swift version{{.*}},{{.*}} flags: "{{[^"]*}}-emit-ir
// CHECK-DAG: !DISubprogram(name: "main"

// Function type for foo.
// CHECK-DAG: ![[FOOTYPE]] = !DISubroutineType(types: ![[PARAMTYPES:[0-9]+]])
// CHECK-DAG: ![[PARAMTYPES]] = !{!"_TtVSs5Int64", !"_TtVSs5Int64", !"_TtVSs5Int64"}
// Import of the main module.
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[MAINFILE]], entity: ![[MAINMODULE:[0-9]+]], line: 1)
// CHECK-DAG: ![[MAINMODULE]] = !MDModule(name: "basic"

// Import of the swift standard library.
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = !DIFile(filename: "{{.*}}Swift.swiftmodule",
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[SWIFTFILE]], entity: ![[SWIFTMODULE:[0-9]+]])
// CHECK-DAG: ![[SWIFTMODULE]] = !MDModule(name: "Swift"

// DWARF Version
// CHECK-DAG:  i32 2, !"Dwarf Version", i32 3}

// Debug Info Version
// CHECK-DAG:  i32 1, !"Debug Info Version", i32

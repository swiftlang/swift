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
// CHECK-DAG: ret{{.*}}, !dbg ![[RET:[0-9]+]]
// CHECK: define i64 @_TF5basic3fooFTVs5Int64S0__S0_
public
func foo(a: Int64, _ b: Int64) -> Int64 {
     var a = a
     var b = b
     // CHECK: [[A:%.*]] = alloca %Vs5Int64, align {{(4|8)}}
     // CHECK: [[B:%.*]] = alloca %Vs5Int64, align {{(4|8)}}
     // CHECK: [[C:%.*]] = alloca %Vs5Int64, align {{(4|8)}}
     // CHECK: [[AADDR:[%].*]] = alloca i64, align {{(4|8)}}
     // CHECK: [[BADDR:[%].*]] = alloca i64, align {{(4|8)}}
     // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64* [[A]], metadata [[ADI:![0-9]+]], metadata !{{[0-9]+}})
     // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64* [[B]], metadata [[BDI:![0-9]+]], metadata !{{[0-9]+}})
     // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64* [[C]], metadata [[CDI:![0-9]+]], metadata !{{[0-9]+}})
     // CHECK: store i64 %0, i64* [[AADDR]], align {{(4|8)}}
     // CHECK: call void @llvm.dbg.declare(metadata i64* [[AADDR]], metadata [[AARGDI:![0-9]+]], metadata !{{[0-9]+}})
     // CHECK: store i64 %1, i64* [[BADDR]], align {{(4|8)}}
     // CHECK: call void @llvm.dbg.declare(metadata i64* [[BADDR]], metadata [[BARGDI:![0-9]+]], metadata !{{[0-9]+}})
     // CHECK: [[AVAL:%.*]] = getelementptr inbounds %Vs5Int64, %Vs5Int64* [[A]], i32 0, i32 0
     // CHECK: store i64 %0, i64* [[AVAL]], align {{(4|8)}}
     // CHECK: [[BVAL:%.*]] = getelementptr inbounds %Vs5Int64, %Vs5Int64* [[B]], i32 0, i32 0
     // CHECK: store i64 %1, i64* [[BVAL]], align {{(4|8)}}

     if b != 0 {
       // CHECK-DAG: !DILexicalBlock({{.*}} line: [[@LINE-1]]
       // Transparent inlined multiply:
       // CHECK-DAG: smul{{.*}}, !dbg ![[MUL:[0-9]+]]
       // CHECK-DAG: [[MUL]] = !DILocation(line: [[@LINE+4]], column: 16,
       // Runtime call to multiply function:
       // CHECK-NOSIL: @_TZFsoi1mFTVs5Int64S__S_{{.*}}, !dbg ![[MUL:[0-9]+]]
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

// CHECK-DAG: ![[FOO:[0-9]+]] = distinct !DISubprogram(name: "foo",{{.*}} line: 26,{{.*}} type: ![[FOOTYPE:[0-9]+]]
// CHECK-DAG: [[ADI]] = !DILocalVariable(name: "a", scope
// CHECK-DAG: [[BDI]] = !DILocalVariable(name: "b", scope
// CHECK-DAG: [[CDI]] = !DILocalVariable(name: "c", scope
// CHECK-DAG: [[AARGDI:![0-9]+]] = !DILocalVariable(name: "a", arg: 1
// CHECK-DAG: [[BARGDI:![0-9]+]] = !DILocalVariable(name: "b", arg: 2

// CHECK-DAG: ![[FILE_CWD:[0-9]+]] = !DIFile(filename: "{{.*}}DebugInfo/basic.swift", directory: "{{.*}}")
// CHECK-DAG: ![[MAINFILE:[0-9]+]] = !DIFile(filename: "basic.swift", directory: "{{.*}}DebugInfo")
// CHECK-DAG: !DICompileUnit(language: DW_LANG_Swift, file: ![[FILE_CWD]],{{.*}} producer: "{{.*}}Swift version{{.*}},{{.*}} flags: "{{[^"]*}}-emit-ir
// CHECK-DAG: !DISubprogram(name: "main"

// Function type for foo.
// CHECK-DAG: ![[FOOTYPE]] = !DISubroutineType(types: ![[PARAMTYPES:[0-9]+]])
// CHECK-DAG: ![[PARAMTYPES]] = !{!"_TtVs5Int64", !"_TtVs5Int64", !"_TtVs5Int64"}
// Import of the main module with the implicit name.
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[MAINFILE]], entity: ![[MAINMODULE:[0-9]+]], line: 1)
// CHECK-DAG: ![[MAINMODULE]] = !DIModule({{.*}}, name: "basic"

// DWARF Version
// CHECK-DAG:  i32 2, !"Dwarf Version", i32 3}

// Debug Info Version
// CHECK-DAG:  i32 2, !"Debug Info Version", i32


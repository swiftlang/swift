// See <rdar://problem/16533351>
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
// CHECK-DAG: ![[FOO:[0-9]+]] ={{.*}}!"0x2e\00{{[^"]+}}\00[[@LINE+2]]"{{, [^,]+, [^,]+}}, ![[FOOTYPE:[0-9]+]],{{.*}} [ DW_TAG_subprogram ] {{.*}} [foo]
public
func foo(var a: Int64, var b: Int64) -> Int64 {
     // CHECK-DAG: !"0xb\00[[@LINE-1]]\0047\000"{{, [^,]+}}, ![[FOO]]} ; [ DW_TAG_lexical_block ]
     // CHECK-DAG: ![[ASCOPE:.*]] = !MDLocation(line: [[@LINE-2]], column: 14, scope: ![[FOO]])
     // Check that a is the first and b is the second argument.
     // CHECK-DAG: store i64 %0, i64* [[AVAL:.*]], align 8
     // CHECK-DAG: store i64 %1, i64* [[BVAL:.*]], align 8
     // CHECK-DAG: [[AVAL]] = getelementptr inbounds {{.*}}, [[AMEM:.*]], i32 0, i32 0
     // CHECK-DAG: [[BVAL]] = getelementptr inbounds {{.*}}, [[BMEM:.*]], i32 0, i32 0
     // CHECK-DAG: call void @llvm.dbg.declare(metadata [[AMEM]], metadata ![[AARG:.*]], metadata !{{[0-9]+}}), !dbg ![[ASCOPE]]
     // CHECK-DAG: call void @llvm.dbg.declare(metadata [[BMEM]], metadata ![[BARG:.*]], metadata !{{[0-9]+}})
     // CHECK-DAG: ![[AARG]] ={{.*}}[ DW_TAG_arg_variable ] [a]
     // CHECK-DAG: ![[BARG]] ={{.*}}[ DW_TAG_arg_variable ] [b]
     if b != 0 {
       // CHECK-DAG: \00[[@LINE-1]]\00{{.*}}DW_TAG_lexical_block
       // Transparent inlined multiply:
       // CHECK-DAG: smul{{.*}}, !dbg ![[MUL:[0-9]+]]
       // CHECK-DAG: [[MUL]] = !MDLocation(line: [[@LINE+4]], column: 16,
       // Runtime call to multiply function:
       // CHECK-NOSIL: @_TZFSsoi1mFTVSs5Int64S__S_{{.*}}, !dbg ![[MUL:[0-9]+]]
       // CHECK-NOSIL: [[MUL]] = !MDLocation(line: [[@LINE+1]], column: 16,
       return a*b
     } else {
       // CHECK-DAG: [[PARENT:[0-9]+]] = {{.*}}\00[[@LINE-1]]\0013\00{{.*}}DW_TAG_lexical_block
       var c: Int64 = 42
       if a == 0 {
         // CHECK-DAG: \00[[@LINE-1]]\0018\00{{.*}}, ![[PARENT]]} ; [ DW_TAG_lexical_block ]
         // What about a nested scope?
         return 0
       }
       return c
     }
}
// CHECK-DAG: Swift version{{.*}}-emit-ir{{.*}} [ DW_TAG_compile_unit ] [{{.*}}basic.swift]
// CHECK-DAG: \00main\00{{.*}} [ DW_TAG_subprogram ]

// Function type for foo.
// CHECK-DAG: ![[FOOTYPE]] = {{.*}} ![[PARAMTYPES:[0-9]+]],{{.*}}DW_TAG_subroutine_type
// CHECK-DAG: ![[PARAMTYPES]] = !{!"_TtVSs5Int64", !"_TtVSs5Int64", !"_TtVSs5Int64"}
// Import of the main module.
// CHECK-DAG: ![[MAINFILE:[0-9]+]] = {{.*}}DW_TAG_file_type{{.*}}basic.swift
// CHECK-DAG: \001\00"{{.*}}, ![[MAINFILE]], ![[MAINMODULE:[0-9]+]]} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[MAINMODULE]] = {{.*}}[ DW_TAG_module ] [basic]

// Import of the swift standard library.
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = {{.*}}DW_TAG_file_type{{.*}}Swift.swiftmodule
// CHECK-DAG: \000\00"{{.*}}, ![[SWIFTFILE]], ![[SWIFTMODULE:[0-9]+]]} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[SWIFTMODULE]] = {{.*}}[ DW_TAG_module ] [Swift]

// Filenames
// CHECK-DAG: {!"basic.swift", !"{{.*}}DebugInfo"}

// DWARF Version
// CHECK-DAG:  i32 2, !"Dwarf Version", i32 3}

// Debug Info Version
// CHECK-DAG:  i32 1, !"Debug Info Version", i32

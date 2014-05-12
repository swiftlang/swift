// See <rdar://problem/16533351>
// A (no longer) basic test for debug info.
// --------------------------------------------------------------------
// Verify that we don't emit any debug info by default.
// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -o - | FileCheck %s --check-prefix NDEBUG
// NDEBUG-NOT: !dbg
// NDEBUG-NOT: DW_TAG
// --------------------------------------------------------------------
// Now check that we do generate line+scope info with -g.
// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - -disable-sil-linking | FileCheck %s --check-prefix=CHECK-NOSIL
// --------------------------------------------------------------------
//
// CHECK: foo
// CHECK-DAG: ret{{.*}}, !dbg ![[RET:[0-9]+]]
// CHECK-DAG: ![[FOO:[0-9]+]] ={{.*}}i32 [[@LINE+1]], metadata ![[FOOTYPE:[0-9]+]],{{.*}} [ DW_TAG_subprogram ] {{.*}} [foo]
func foo(var a: Int, var b: Int) -> Int {
     // CHECK-DAG: metadata ![[FOO]], i32 [[@LINE-1]], i32 41{{.*}}DW_TAG_lexical_block
     // CHECK-DAG: ![[ASCOPE:.*]] = metadata !{i32 [[@LINE-2]], i32 14, metadata ![[FOO]], null}
     // Check that a is the first and b is the second argument.
     // CHECK-DAG: store i64 %0, i64* [[AVAL:.*]], align 8
     // CHECK-DAG: store i64 %1, i64* [[BVAL:.*]], align 8
     // CHECK-DAG: [[AVAL]] = getelementptr inbounds [[AMEM:.*]], i32 0, i32 0
     // CHECK-DAG: [[BVAL]] = getelementptr inbounds [[BMEM:.*]], i32 0, i32 0
     // CHECK-DAG: call void @llvm.dbg.declare(metadata !{[[AMEM]]}, metadata ![[AARG:.*]]), !dbg ![[ASCOPE]]
     // CHECK-DAG: call void @llvm.dbg.declare(metadata !{[[BMEM]]}, metadata ![[BARG:.*]])
     // CHECK-DAG: ![[AARG]] ={{.*}}[ DW_TAG_arg_variable ] [a]
     // CHECK-DAG: ![[BARG]] ={{.*}}[ DW_TAG_arg_variable ] [b]
     if b != 0 {
       // CHECK-DAG: i32 [[@LINE-1]],{{.*}}DW_TAG_lexical_block
       // Transparent inlined multiply:
       // CHECK-DAG: smul{{.*}}, !dbg ![[DIV:[0-9]+]]
       // CHECK-DAG: [[DIV]] = metadata !{i32 [[@LINE+4]], i32 15,
       // Runtime call to multiply function:
       // CHECK-NOSIL: @_TFSsoi1mFTSiSi_Si{{.*}}, !dbg ![[DIV:[0-9]+]]
       // CHECK-NOSIL: [[DIV]] = metadata !{i32 [[@LINE+1]], i32 15,
       return a*b
     } else {
       // CHECK-DAG: [[PARENT:[0-9]+]] = {{.*}}metadata !{{[0-9]+}}, i32 [[@LINE-1]], i32 13{{.*}}DW_TAG_lexical_block
       var c = 42
       if a == 0 {
         // CHECK-DAG: metadata ![[PARENT]], i32 [[@LINE-1]], i32 18{{.*}}DW_TAG_lexical_block
         // What about a nested scope?
         return 0
       }
       return c
     }
}
// CHECK-DAG: Swift version{{.*}}x86_64-apple-darwin10{{.*}}-emit-ir{{.*}} [ DW_TAG_compile_unit ] [{{.*}}basic.swift]
// top_level_code is not an artificial function (Flags & 64 == 0).
//            LinkageName       Line   Type                  local     isDefn   0      0  ScopeLine Flags   optimized
// CHECK-DAG: "top_level_code", i32 1, metadata !{{[0-9]+}}, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void{{.*}} [ DW_TAG_subprogram ]
// CHECK-DAG: "main"{{.*}} [ DW_TAG_subprogram ]

// Function type for foo.
// CHECK-DAG: ![[FOOTYPE]] = {{.*}} metadata ![[PARAMTYPES:[0-9]+]],{{.*}}DW_TAG_subroutine_type
// CHECK-DAG: ![[PARAMTYPES]] = metadata !{metadata ![[R:[0-9]+]], metadata ![[A:[0-9]+]], metadata ![[B:[0-9]+]]}
// CHECK-DAG: ![[R]] = {{.*}}"_TtSi"
// CHECK-DAG: ![[A]] = {{.*}}"_TtSi"
// CHECK-DAG: ![[B]] = {{.*}}"_TtSi"

// Import of the main module.
// CHECK-DAG: ![[MAINFILE:[0-9]+]] = {{.*}}DW_TAG_file_type{{.*}}basic.swift
// CHECK-DAG: metadata ![[MAINFILE]], metadata ![[MAINMODULE:[0-9]+]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[MAINMODULE]] = {{.*}}[ DW_TAG_module ] [basic]

// Import of the swift standard library.
// CHECK-DAG: ![[SWIFTFILE:[0-9]+]] = {{.*}}DW_TAG_file_type{{.*}}Swift.swiftmodule
// CHECK-DAG: metadata ![[SWIFTFILE]], metadata ![[SWIFTMODULE:[0-9]+]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK-DAG: ![[SWIFTMODULE]] = {{.*}}[ DW_TAG_module ] [Swift]

// Filenames
// CHECK-DAG: {metadata !"basic.swift", metadata !"{{.*}}DebugInfo"}

// DWARF Version
// CHECK-DAG: metadata !{i32 2, metadata !"Dwarf Version", i32 3}

// Debug Info Version
// CHECK-DAG: metadata !{i32 1, metadata !"Debug Info Version", i32

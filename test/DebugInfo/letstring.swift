// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o %t.ll
// RUN: cat %t.ll | FileCheck %s
class UIWindow {}
class AppDelegate {
  var window: UIWindow?
  // CHECK: define i1 {{.*}}11AppDelegate1f
  func f() -> Bool {
    // Test for -O0 shadow copies.
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[B:.*]])
    // CHECK-NOT: dbg.value
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[SELF:.*]])
    let a = "let"
    // CHECK-NOT: dbg.value
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[A:.*]])
    // CHECK-DAG: ![[A]] = {{.*}}; [ DW_TAG_auto_variable ] [a] [line [[@LINE-3]]]
    // CHECK-DAG: ![[SELF]] = {{.*}}; [ DW_TAG_arg_variable ] [self] [line [[@LINE-9]]]
    // CHECK-DAG: ![[B]] = {{.*}}; [ DW_TAG_auto_variable ] [b] [line [[@LINE+1]]]
    var b = "var"
    self.window = UIWindow()
    return true
  }
}

// End-to-end test:
// RUN: llc %t.ll -filetype=obj -o %t.o
// RUN: llvm-dwarfdump %t.o | FileCheck %s --check-prefix DWARF-CHECK
// DWARF-CHECK: DW_AT_name {{.*}} "f"
//
// DWARF-CHECK: DW_TAG_formal_parameter
// DWARF-CHECK:  DW_AT_name {{.*}} "self"
//
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "b"
//
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "a"

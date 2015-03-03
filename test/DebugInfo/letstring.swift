// RUN: %target-swift-frontend %s -emit-ir -g -o %t.ll
// RUN: FileCheck %s < %t.ll

class UIWindow {}
class AppDelegate {
  var window: UIWindow?
  // CHECK: define hidden i1 {{.*}}11AppDelegate1f
  func f() -> Bool {
    // Test for -O0 shadow copies.
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[B:.*]], metadata !{{[0-9]+}})
    // CHECK-NOT: call void @llvm.dbg.value({{.*}}, metadata ![[B]], metadata !{{[0-9]+}})
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    let a = "let"
    // CHECK-NOT: call void @llvm.dbg.value({{.*}}, metadata ![[SELF]], metadata !{{[0-9]+}})
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[A:.*]], metadata !{{[0-9]+}})
    // CHECK-NOT: call void @llvm.dbg.value({{.*}}, metadata ![[A]], metadata !{{[0-9]+}})
    // CHECK-DAG: ![[A]] = !MDLocalVariable(tag: DW_TAG_auto_variable, name: "a",{{.*}} line: [[@LINE-4]],
    // CHECK-DAG: ![[SELF]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "self",{{.*}} line: [[@LINE-10]],
    // CHECK-DAG: ![[B]] = !MDLocalVariable(tag: DW_TAG_auto_variable, name: "b",{{.*}} line: [[@LINE+1]],
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

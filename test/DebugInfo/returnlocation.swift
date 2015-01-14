// RUN: %target-build-swift -g -emit-ir %s -o %t.ll

import Foundation

// This file contains linetable testcases for all permutations
// of simple/complex/empty return expressions,
// cleanups/no cleanups, single / multiple return locations.

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_NONE
// CHECK_NONE: define hidden void {{.*}}none
func none(inout a: Int) {
  // CHECK_NONE: call void @llvm.dbg{{.*}}, !dbg
  // CHECK_NONE: !dbg ![[NONE_INIT:.*]]
  a -= 2;
  // CHECK_NONE: ret {{.*}}, !dbg ![[NONE_RET:.*]]
  // CHECK_NONE: ![[NONE_INIT]] = !MDLocation(line: [[@LINE-2]], column:
  // CHECK_NONE: ![[NONE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_EMPTY
// CHECK_EMPTY: define hidden {{.*}}empty
func empty(inout a: Int) {
  if a > 24 {
      // CHECK-DAG_EMPTY: br {{.*}}, !dbg ![[EMPTY_RET1:.*]]
      // CHECK-DAG_EMPTY_RET1: ![[EMPTY_RET1]] = !MDLocation(line: [[@LINE+1]], column: 6,
      return
  }

  a -= 2;
  // CHECK-DAG_EMPTY: br {{.*}}, !dbg ![[EMPTY_RET2:.*]]
  // CHECK-DAG_EMPTY_RET2: ![[EMPTY_RET]] = !MDLocation(line: [[@LINE+1]], column: 3,
  return
  // CHECK-DAG_EMPTY: ret {{.*}}, !dbg ![[EMPTY_RET:.*]]
  // CHECK-DAG_EMPTY: ![[EMPTY_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_EMPTY_NONE
// CHECK_EMPTY_NONE: define {{.*}}empty_none
func empty_none(inout a: Int) {
  if a > 24 {
      return;
  }

  a -= 2;
  // CHECK_EMPTY_NONE: ret {{.*}}, !dbg ![[EMPTY_NONE_RET:.*]]
  // CHECK_EMPTY_NONE: ![[EMPTY_NONE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_SIMPLE_RET
// CHECK_SIMPLE_RET: define {{.*}}simple
func simple(a: Int) -> Int {
  if a > 24 {
      return 0;
  }
  return 1
  // CHECK_SIMPLE_RET: ret i{{.*}}, !dbg ![[SIMPLE_RET:.*]]
  // CHECK_SIMPLE_RET: ![[SIMPLE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_COMPLEX_RET
// CHECK_COMPLEX_RET: define {{.*}}complex
func complex(a: Int) -> Int {
  if a > 24 {
      return a*a
  }
  return a/2
  // CHECK_COMPLEX_RET: ret i{{.*}}, !dbg ![[COMPLEX_RET:.*]]
  // CHECK_COMPLEX_RET: ![[COMPLEX_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_COMPLEX_SIMPLE
// CHECK_COMPLEX_SIMPLE: define {{.*}}complex_simple
func complex_simple(a: Int) -> Int {
  if a > 24 {
      return a*a
  }
  return 2
  // CHECK_COMPLEX_SIMPLE: ret i{{.*}}, !dbg ![[COMPLEX_SIMPLE_RET:.*]]
  // CHECK_COMPLEX_SIMPLE: ![[COMPLEX_SIMPLE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_SIMPLE_COMPLEX
// CHECK_SIMPLE_COMPLEX: define {{.*}}simple_complex
func simple_complex(a: Int) -> Int {
  if a > 24 {
      return a*a
  }
  return 2
  // CHECK_SIMPLE_COMPLEX: ret {{.*}}, !dbg ![[SIMPLE_COMPLEX_RET:.*]]
  // CHECK_SIMPLE_COMPLEX: ![[SIMPLE_COMPLEX_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}


// ---------------------------------------------------------------------


// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_NONE
// CHECK_CLEANUP_NONE: define {{.*}}cleanup_none
func cleanup_none(inout a: NSString) {
  a = "empty"
  // CHECK_CLEANUP_NONE: ret void, !dbg ![[CLEANUP_NONE_RET:.*]]
  // CHECK_CLEANUP_NONE: ![[CLEANUP_NONE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_EMPTY
// CHECK_CLEANUP_EMPTY: define {{.*}}cleanup_empty
func cleanup_empty(inout a: NSString) {
  if a.length > 24 {
      return;
    }

  a = "empty"
  return
  // CHECK_CLEANUP_EMPTY: ret void, !dbg ![[CLEANUP_EMPTY_RET:.*]]
  // CHECK_CLEANUP_EMPTY: ![[CLEANUP_EMPTY_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_EMPTY_NONE
// CHECK_CLEANUP_EMPTY_NONE: define {{.*}}cleanup_empty_none
func cleanup_empty_none(inout a: NSString) {
  if a.length > 24 {
      return;
    }

  a = "empty"
  // CHECK_CLEANUP_EMPTY_NONE: ret {{.*}}, !dbg ![[CLEANUP_EMPTY_NONE_RET:.*]]
  // CHECK_CLEANUP_EMPTY_NONE: ![[CLEANUP_EMPTY_NONE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_SIMPLE_RET
// CHECK_CLEANUP_SIMPLE_RET: define {{.*}}cleanup_simple
func cleanup_simple(a: NSString) -> Int {
  if a.length > 24 {
      return 0
  }

  return 1
  // CHECK_CLEANUP_SIMPLE_RET: ret {{.*}}, !dbg ![[CLEANUP_SIMPLE_RET:.*]]
  // CHECK_CLEANUP_SIMPLE_RET: ![[CLEANUP_SIMPLE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_COMPLEX
// CHECK_CLEANUP_COMPLEX: define {{.*}}cleanup_complex
func cleanup_complex(a: NSString) -> Int {
  if a.length > 24 {
      return a.length*a.length
  }

  return a.length/2
  // CHECK_CLEANUP_COMPLEX: ret i{{.*}}, !dbg ![[CLEANUP_COMPLEX_RET:.*]]
  // CHECK_CLEANUP_COMPLEX: ![[CLEANUP_COMPLEX_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_COMPLEX_SIMPLE
// CHECK_CLEANUP_COMPLEX_SIMPLE: define {{.*}}cleanup_complex_simple
func cleanup_complex_simple(a: NSString) -> Int {
  if a.length > 24 {
      return a.length*a.length
  }

  return 2
  // CHECK_CLEANUP_COMPLEX_SIMPLE: ret {{.*}}, !dbg ![[CLEANUP_COMPLEX_SIMPLE_RET:.*]]
  // CHECK_CLEANUP_COMPLEX_SIMPLE: ![[CLEANUP_COMPLEX_SIMPLE_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// RUN: cat %t.ll | FileCheck %s --check-prefix=CHECK_CLEANUP_SIMPLE_COMPLEX
// CHECK_CLEANUP_SIMPLE_COMPLEX: define {{.*}}cleanup_simple_complex
func cleanup_simple_complex(a: NSString) -> Int {
  if a.length > 24 {
      return a.length*a.length
  }
  return 2
  // CHECK_CLEANUP_SIMPLE_COMPLEX: ret {{.*}}, !dbg ![[CLEANUP_SIMPLE_COMPLEX_RET:.*]]
  // CHECK_CLEANUP_SIMPLE_COMPLEX: ![[CLEANUP_SIMPLE_COMPLEX_RET]] = !MDLocation(line: [[@LINE+1]], column: 1,
}

// ---------------------------------------------------------------------

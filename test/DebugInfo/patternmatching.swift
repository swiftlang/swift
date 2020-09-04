// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck --check-prefix=CHECK-SCOPES %s < %t.ll
// RUN: %target-swift-frontend -emit-sil -emit-verbose-sil -primary-file %s -o - | %FileCheck %s --check-prefix=SIL-CHECK





// This comment must be at line 10 for the test to work.
func markUsed<T>(_ t: T) {}

// CHECK-SCOPES: define {{.*}}classifyPoint2
func classifyPoint2(_ p: (Double, Double)) {
    func return_same (_ input : Double) -> Double
    {
        return input; // return_same gets called in both where statements
    }

    switch p {
    case (let x, let y) where
      // CHECK:   call {{.*}}double {{.*}}return_same{{.*}}, !dbg ![[LOC1:.*]]
      // CHECK: br {{.*}}, label {{.*}}, label {{.*}}, !dbg ![[LOC2:.*]]
      // CHECK: call{{.*}}markUsed{{.*}}, !dbg ![[LOC3:.*]]
      // CHECK: ![[LOC1]] = !DILocation(line: [[@LINE+2]],
      // CHECK: ![[LOC2]] = !DILocation(line: [[@LINE+1]],
                        return_same(x) == return_same(y):
      // CHECK: ![[LOC3]] = !DILocation(line: [[@LINE+1]],
      markUsed(x)
      // SIL-CHECK:  dealloc_stack{{.*}}line:[[@LINE-1]]:17:cleanup
      // Verify that the branch has a location >= the cleanup.
      // SIL-CHECK-NEXT:  br{{.*}}auto_gen
    case (let x, let y) where x == -y:
      markUsed(x)
    case (let x, let y) where x >= -10 && x < 10 && y >= -10 && y < 10:
      markUsed(x)
    case (let x, let y):
      markUsed(x)
    }
    switch p {
    case (let x, let y) where x == 0:
      if y == 0 { markUsed(x) }
      else      { markUsed(y) } // SIL-CHECK-NOT: br{{.*}}line:[[@LINE]]:31:cleanup
    case (let x, let y): do {
      if y == 0 { markUsed(x) }
      else      { markUsed(y) }
    } // SIL-CHECK: br{{.*}}line:[[@LINE]]:5:cleanup
    }

// Test the scopes for the switch at line 20.

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[P:[0-9]+]],

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X1:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X1LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y1:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y1LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X2:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X2LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y2:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y2LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X3:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X3LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y3:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y3LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X4:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X4LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y4:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y4LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X5:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X5LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y5:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y5LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X6:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[X6LOC:[0-9]+]]

// CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[Y6:[0-9]+]],
// CHECK-SCOPES-SAME:                         !dbg ![[Y6LOC:[0-9]+]]

// Verify that variables end up in separate appropriate scopes.

// CHECK-SCOPES: ![[X1]] = {{.*}}name: "x", scope: ![[SCOPE1:[0-9]+]], {{.*}}line: 37
// CHECK-SCOPES: ![[SCOPE1]] = distinct !DILexicalBlock(scope: ![[SWITCH1:[0-9]+]], {{.*}}line: 37
// CHECK-SCOPES: ![[SWITCH1]] = distinct !DILexicalBlock({{.*}}, line: 20
// CHECK-SCOPES: ![[X1LOC]] = {{.*}}line: 37

// CHECK-SCOPES: ![[Y1]] = {{.*}}name: "y", scope: ![[SCOPE1]], {{.*}}line: 37
// CHECK-SCOPES: ![[Y1LOC]] = {{.*}}line: 37

// CHECK: !DILocation(line: [[@LINE+1]],
}

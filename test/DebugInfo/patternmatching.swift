// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck --check-prefix=CHECK-SCOPES %s < %t.ll
// RUN: %target-swift-frontend -emit-sil -emit-verbose-sil -primary-file %s -o - | %FileCheck %s --check-prefix=SIL-CHECK

func markUsed<T>(_ t: T) {}

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
      // SIL-CHECK-NEXT:  br{{.*}}line:[[@LINE-3]]:17:cleanup
      // CHECK-SCOPES: call {{.*}}markUsed
      // CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X1:[0-9]+]],
      // CHECK-SCOPES-SAME:                         !dbg ![[X1LOC:[0-9]+]]
      // CHECK-SCOPES: call void @llvm.dbg
      // CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X2:[0-9]+]],
      // CHECK-SCOPES-SAME:                         !dbg ![[X2LOC:[0-9]+]]
      // CHECK-SCOPES: call void @llvm.dbg
      // CHECK-SCOPES: call void @llvm.dbg{{.*}}metadata ![[X3:[0-9]+]],
      // CHECK-SCOPES-SAME:                         !dbg ![[X3LOC:[0-9]+]]
      // CHECK-SCOPES: !DILocalVariable(name: "x",
    case (let x, let y) where x == -y:
      // Verify that all variables end up in separate appropriate scopes.
      // CHECK-SCOPES: !DILocalVariable(name: "x", scope: ![[SCOPE1:[0-9]+]],
      // CHECK-SCOPES-SAME:             line: [[@LINE-3]]
      // CHECK-SCOPES: ![[X1LOC]] = !DILocation(line: [[@LINE-4]], column: 15,
      // CHECK-SCOPES-SAME:                     scope: ![[SCOPE1]])
      // FIXME: ![[SCOPE1]] = distinct !DILexicalBlock({{.*}}line: [[@LINE-6]]
      markUsed(x)
    case (let x, let y) where x >= -10 && x < 10 && y >= -10 && y < 10:
      // CHECK-SCOPES: !DILocalVariable(name: "x", scope: ![[SCOPE2:[0-9]+]],
      // CHECK-SCOPES-SAME:             line: [[@LINE-2]]
      // CHECK-SCOPES: ![[X2LOC]] = !DILocation(line: [[@LINE-3]], column: 15,
      // CHECK-SCOPES-SAME:                     scope: ![[SCOPE2]])
      markUsed(x)
    case (let x, let y):
      // CHECK-SCOPES: !DILocalVariable(name: "x", scope: ![[SCOPE3:[0-9]+]],
      // CHECK-SCOPES-SAME:             line: [[@LINE-2]]
      // CHECK-SCOPES: ![[X3LOC]] = !DILocation(line: [[@LINE-3]], column: 15,
      // CHECK-SCOPES-SAME:                     scope: ![[SCOPE3]])
      markUsed(x)
    }
// CHECK: !DILocation(line: [[@LINE+1]],
}

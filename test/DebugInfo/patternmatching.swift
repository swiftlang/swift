// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir -g -o %t
// RUN: cat %t | FileCheck %s
// RUN: cat %t | FileCheck --check-prefix=CHECK-SCOPES %s
// RUN: %swift -emit-sil -emit-verbose-sil -primary-file %s -o - | FileCheck %s --check-prefix=SIL-CHECK
func classifyPoint2(p: (Double, Double)) {
    func return_same (var input : Double) -> Double
    {
        return input; // return_same gets called in both where statements
    }


    switch p {
        case (0, 0):
          println("origin")
        case (0, _):
          println("on the Y axis")
          println("(0, \(p.1)) is on the y-axis")
        case (_, 0):
          println("on the X axis")
        case (var x, var y) where
          // CHECK:   call double {{.*}}return_same{{.*}}, !dbg ![[LOC1:.*]]
          // CHECK: br {{.*}}, label {{.*}}, label {{.*}}, !dbg ![[LOC2:.*]]
          // CHECK: builtinStringLiteral{{.*}}, !dbg ![[LOC3:.*]]
          // CHECK: ![[LOC1]] = !MDLocation(line: [[@LINE+2]],
          // CHECK: ![[LOC2]] = !MDLocation(line: [[@LINE+1]],
                            return_same(x) == return_same(y):
          // CHECK: ![[LOC3]] = !MDLocation(line: [[@LINE+1]],
          println("(\(x), \(y)) is on the + diagonal")
          // SIL-CHECK:  dealloc_stack{{.*}}line:[[@LINE-1]]:54:cleanup
          // Verify that the branch has a location >= the cleanup.
          // SIL-CHECK-NEXT:  br{{.*}}line:[[@LINE-3]]:54:cleanup
        case (var x, var y) where x == -y:
          // Verify that all variables end up in the appropriate scopes.
          // CHECK-SCOPES: ", ![[SCOPE1:[0-9]+]], {{.*}} ; [ DW_TAG_auto_variable ] [x] {{.}}line [[@LINE-2]]
          // CHECK-SCOPES: ![[SCOPE1]] = {{.*}}; [ DW_TAG_lexical_block ]
          println("on the - diagonal")
        case (var x, var y) where x >= -10 && x < 10 && y >= -10 && y < 10:
          // CHECK-SCOPES: ", ![[SCOPE2:[0-9]+]], {{.*}} ; [ DW_TAG_auto_variable ] [x] {{.}}line [[@LINE-1]]
          // CHECK-SCOPES: ![[SCOPE2]] = {{.*}}; [ DW_TAG_lexical_block ]
          println("near the origin")
        case (var x, var y):
          // CHECK-SCOPES: ", ![[SCOPE3:[0-9]+]], {{.*}} ; [ DW_TAG_auto_variable ] [x] {{.}}line [[@LINE-1]]
          // CHECK-SCOPES: ![[SCOPE3]] = {{.*}} ; [ DW_TAG_lexical_block ]
          println("sqrt(\(x*x + y*y)) units from the origin")
        }
  // CHECK: !MDLocation(line: [[@LINE+1]],
    }

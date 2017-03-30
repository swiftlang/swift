// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class Person {
    var name = "No Name"
    var age = 0
}

func main() {
    var person = Person()
    var b = [0,1,13]
    for element in b {
        markUsed("element = \(element)")
    }
    markUsed("Done with the for loop")
// CHECK: call {{.*}}void @_T04main8markUsedyxlF
// CHECK: br label
// CHECK: <label>:
// CHECK: , !dbg ![[LOOPHEADER_LOC:.*]]
// CHECK: call void {{.*[rR]}}elease{{.*}} {{#[0-9]+}}, !dbg ![[LOOPHEADER_LOC]]
// CHECK: call {{.*}}void @_T04main8markUsedyxlF
// The cleanups should share the line number with the ret stmt.
// CHECK:  call void {{.*[rR]}}elease{{.*}} {{#[0-9]+}}, !dbg ![[CLEANUPS:.*]]
// CHECK-NEXT:  !dbg ![[CLEANUPS]]
// CHECK-NEXT:  bitcast
// CHECK-NEXT:  llvm.lifetime.end
// CHECK-NEXT:  bitcast
// CHECK-NEXT:  llvm.lifetime.end
// CHECK-NEXT:  bitcast
// CHECK-NEXT:  llvm.lifetime.end
// CHECK-NEXT:  ret void, !dbg ![[CLEANUPS]]
// CHECK: ![[CLEANUPS]] = !DILocation(line: [[@LINE+1]], column: 1,
}
main()

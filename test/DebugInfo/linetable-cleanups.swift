// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class Person {
    var name = "No Name"
    var age = 0
}

func main() {
    var person = Person()
    var b = [0,1,13]
    for element in b {
        println("element = \(element)")
    }
    println("Done with the for loop")
// CHECK: call void @_TFSs7println
// CHECK: br label
// CHECK: <label>:
// CHECK: , !dbg ![[LOOPHEADER_LOC:.*]]
// CHECK: call void {{.*}}elease({{.*}}) {{#[0-9]+}}, !dbg ![[LOOPHEADER_LOC]]
// CHECK: call void @_TFSs7println
// The cleanups should share the line number with the ret stmt.
// CHECK:  call void {{.*}}elease({{.*}}) {{#[0-9]+}}, !dbg ![[CLEANUPS:.*]]
// CHECK-NEXT:  !dbg ![[CLEANUPS]]
// CHECK-NEXT:  ret void, !dbg ![[CLEANUPS]]
// CHECK: ![[CLEANUPS]] = !MDLocation(line: [[@LINE+1]], column: 1,
}
main()

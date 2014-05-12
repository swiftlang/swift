// RUN: %swift -target x86_64-apple-darwin %s -emit-ir -g -o - | FileCheck %s

class Person {
    var name = "No Name"
    var age = 0
}

func main() {
  // The inlined release for Person should share the location with the instruction before it.
  // CHECK: call void @swift_retain_noresult{{.*}}, !dbg ![[LOC:.*]]
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
// CHECK: call void {{.*}}_release({{.*}}) #1, !dbg ![[LOOPHEADER_LOC]]
// CHECK: call void @_TFSs7println
// The cleanups should share the line number with the ret stmt.
// CHECK:  call void {{.*}}_release({{.*}}) #1, !dbg ![[CLEANUPS:.*]]
// CHECK-NEXT:  !dbg ![[CLEANUPS]]
// CHECK-NEXT:  ret void, !dbg ![[CLEANUPS]]
// CHECK: ![[CLEANUPS]] = metadata !{i32 [[@LINE+1]], i32 1,
}
main()

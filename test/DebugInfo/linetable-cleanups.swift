// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

func main() {
    var b = [0,1,13]
    for element in b {
        println("element = \(element)")
    }
    println("Done with the for loop")
// CHECK:  call void @_TSs7printlnFT3valSS_T_
// CHECK:  call void @_TSs7printlnFT3valSS_T_
// The cleanups should share the line number with the ret stmt.
// CHECK-NEXT:  call void @swift_release(%swift.refcounted* %{{.*}}) #1, !dbg ![[CLEANUPS:.*]]
// CHECK-NEXT:  ret void, !dbg ![[CLEANUPS]]
// CHECK: ![[CLEANUPS]] = metadata !{i32 [[@LINE+1]], i32 1,
}
main()


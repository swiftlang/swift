// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

func foo(inout x : Int) {
  // Make sure the shadow copy is being made in the prologue, but the
  // code to load the value from the inout storage is not.
  x = x + 2
  // CHECK: %[[X:.*]] = alloca %Si*, align {{(4|8)}}
  // CHECK: store %Si* %0, %Si** %[[X]], align {{(4|8)}}
  // CHECK-NOT: !dbg
  // CHECK-NEXT: call void @llvm.dbg.declare
  // CHECK-NEXT: getelementptr inbounds %Si, %Si* %0, i32 0, i32 0, !dbg
}

func main() {
  var x = 1
  foo(&x)
  markUsed("break here to see \(x)")
}

main()

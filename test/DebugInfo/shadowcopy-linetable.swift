// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

func foo(inout x : Int64) {
  // Make sure the shadow copy is being made in the prologue, but the
  // code to load the value from the inout storage is not.
  x = x + 2
  // CHECK: %[[X:.*]] = alloca %Vs5Int64*, align {{(4|8)}}
  // CHECK: store %Vs5Int64* %0, %Vs5Int64** %[[X]], align {{(4|8)}}
  // CHECK-NOT: !dbg
  // CHECK-NEXT: call void @llvm.dbg.declare
  // CHECK-NEXT: getelementptr inbounds %Vs5Int64, %Vs5Int64* %0, i32 0, i32 0, !dbg
}

func main() {
  var x : Int64 = 1
  foo(&x)
  markUsed("break here to see \(x)")
}

main()

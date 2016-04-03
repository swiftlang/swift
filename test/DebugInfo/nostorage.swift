// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(_ t: T) {}

class AClass {
  func f () -> Int64 { return 1 }
}

class AnotherClass : AClass {
  override func f() -> Int64 { return 2 }
}

struct AStruct {
  func f() -> Int64 { return 3 }
}

// CHECK: define hidden void @_TF9nostorage3appFT_T_()
func app() {
  var ac: AClass = AnotherClass()
  // No members? No storage! Emitted as a constant 0, because.
  // CHECK: call void @llvm.dbg.value(metadata {{.*}}, i64 0, metadata ![[AT:.*]], metadata !{{[0-9]+}}), !dbg
  // CHECK: ![[AT]] = !DILocalVariable(name: "at",
  // CHECK-SAME:                       line: [[@LINE+1]]
  var at = AStruct()
  markUsed("\(ac.f()) \(at.f())")
}

app()

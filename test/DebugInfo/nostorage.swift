// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class AClass {
  func f () -> Int { return 1 }
}

class AnotherClass : AClass {
  override func f() -> Int { return 2 }
}

struct AStruct {
  func f() -> Int { return 3 }
}

// CHECK: define hidden void @_TF9nostorage3appFT_T_()
func app() {
	var ac : AClass = AnotherClass()
        // No members? No storage! Emitted as a constant 0, because.
        // CHECK: call void @llvm.dbg.value(metadata {{.*}}, i64 0, metadata ![[AT:.*]], metadata !{{[0-9]+}}), !dbg
        // CHECK: ![[AT]] = {{.*}}[ DW_TAG_auto_variable ] [at] [line [[@LINE+1]]]
	var at = AStruct()
	println("\(ac.f()) \(at.f())")
}

app()

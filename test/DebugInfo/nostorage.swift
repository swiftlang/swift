// RUN: %target-swift-frontend %s -emit-ir -g -o %t
// RUN: cat %t | FileCheck %s --check-prefix=CHECK1
// RUN: cat %t | FileCheck %s --check-prefix=CHECK2

func used<T>(_ t: T) {}

public class Foo {
    func foo() {
      { [weak self] in
      // CHECK1: call void @llvm.dbg.value(metadata i{{.*}} 0,
      // CHECK1-SAME:                      metadata ![[TYPE:.*]], metadata
      // CHECK1: ![[TYPE]] = !DILocalVariable(name: "type",
      // CHECK1-SAME:                         line: [[@LINE+1]],
            let type = self.dynamicType
            used(type)
        }()
    }
}

struct AStruct {}

// CHECK2: define{{.*}}app
public func app() {
  // No members? No storage! Emitted as a constant 0, because.
  // CHECK2: call void @llvm.dbg.value(metadata i{{.*}} 0,
  // CHECK2-SAME:                      metadata ![[AT:.*]], metadata
  // CHECK2: ![[AT]] = !DILocalVariable(name: "at",{{.*}}line: [[@LINE+1]]
  var at = AStruct()
  
  used(at)
}

// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
enum MyError : Error {
  case Yikes
}

func throwing() throws -> () {
  throw MyError.Yikes
}

func use<T>(_ t: T) {}

public func foo() {
  do {
    try throwing()
  }
  catch let error {
    // CHECK: call void @llvm.dbg.declare(metadata %swift.error** %{{.*}}, metadata ![[ERROR:[0-9]+]],
    // CHECK: ![[ERROR]] = !DILocalVariable(name: "error"
    use(error)
  }
}
foo();

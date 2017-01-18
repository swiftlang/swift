// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// CHECK: define hidden void @_TFC12generic_arg25Class3foo{{.*}}, %swift.type* %U
// CHECK: [[Y:%.*]] = getelementptr inbounds %C12generic_arg25Class, %C12generic_arg25Class* %2, i32 0, i32 0, i32 0
// store %swift.opaque* %[[Y]], %swift.opaque** %[[Y_SHADOW:.*]], align
// CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %y.addr, metadata ![[U:.*]], metadata !{{[0-9]+}})
// Make sure there is no conflicting dbg.value for this variable.x
// CHECK-NOT: dbg.value{{.*}}metadata ![[U]]
class Class <T> {
// CHECK: ![[U]] = !DILocalVariable(name: "y", arg: 2{{.*}} line: [[@LINE+1]],
  func foo<U>(_ x: T, y: U) {}

  func bar(_ x: String, y: Int64) {}

  init() {}
}

func main() {
  var object: Class<String> = Class()
  var x = "hello"
  var y : Int64 = 1234
  object.bar(x, y: y)
  object.foo(x, y: y)
}

main()

// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK: @_TFC12generic_arg25Class3foo{{.*}}, %swift.type* %U
// CHECK: %[[X:[0-9]+]] = call %swift.opaque* %initializeBufferWithCopy{{.*}}([{{(24|12)}} x i8]* %{{.*}}, %swift.opaque* %{{.*}}, %swift.type* %T)
// CHECK-NEXT: call void @llvm.dbg.value(metadata %swift.opaque* %[[X]], {{.*}}metadata ![[T:[0-9]+]], metadata !{{[0-9]+}}), !dbg !{{[0-9]+}}
// CHECK: %[[Y:[0-9]+]] = call %swift.opaque* %initializeBufferWithCopy{{.*}}([{{(24|12)}} x i8]* %{{.*}}, %swift.opaque* %{{.*}}, %swift.type* %U)
// CHECK-NEXT: call void @llvm.dbg.value(metadata %swift.opaque* %[[Y]], {{.*}}metadata ![[U:[0-9]+]], metadata !{{[0-9]+}}), !dbg !{{[0-9]+}}
// Make sure there is no conflicting dbg.value for this variable.x
// CHECK-NOT: dbg.value{{.*}}metadata ![[U]]
class Class <T> {
  func foo<U>(x: T, y: U) {
    // CHECK: ![[T]] = !DILocalVariable(name: "x", scope: !{{[0-9]+}}, {{.*}}line: [[@LINE+1]],
    var x = x
    // CHECK: ![[U]] = !DILocalVariable(name: "y", scope: !{{[0-9]+}}, {{.*}}line: [[@LINE+1]],
    var y = y
    _ = x
    _ = y
  }

  func bar(x: String, y: Int64) {
    var x = x
    var y = y
    _ = x
    _ = y
  }

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

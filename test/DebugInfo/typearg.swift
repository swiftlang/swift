// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

protocol AProtocol {
  func f() -> String;
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}

// CHECK: define hidden void @{{.*}}aFunction
// CHECK:  call void @llvm.dbg.declare(metadata %swift.type** %{{.*}}, metadata ![[TYPEARG:.*]], metadata !{{[0-9]+}}),
// CHECK: ![[TYPEARG]] = !{!"{{.*}}$swift.type.T\000\0064", !{{[^,]+}}, !{{[^,]+}}, ![[SWIFTMETATYPE:.*]]} ; [ DW_TAG_auto_variable ] [$swift.type.T]
// CHECK: ![[SWIFTMETATYPE]] = {{.*}} ; [ DW_TAG_typedef ] [$swift.type] [line 0, size 0, align 0, offset 0] [from _TtBp]
func aFunction<T : AProtocol>(x: T) {
    println("I am in aFunction: \(x.f())")
}

aFunction(AClass())

// Verify that we also emit a swift.type for a generic self.
class Foo<Bar> {
      func one() {
      }

      func two<Baz>(x: Baz) {
    // CHECK: [ DW_TAG_auto_variable ] [$swift.type.Bar]
    // CHECK: [ DW_TAG_auto_variable ] [$swift.type.Baz]
      }
}

// Verify that the backend doesn't elide the debug intrinsics.
// RUN: %target-swift-frontend %s -c -g -o %t.o
// RUN: llvm-dwarfdump %t.o | FileCheck %s --check-prefix=CHECK-LLVM
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "x"
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.T"
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.Bar"
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.Baz"

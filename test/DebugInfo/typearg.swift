// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

protocol AProtocol {
  func f() -> String
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}

// CHECK: define hidden {{.*}}void @{{.*}}aFunction
// CHECK:  call void @llvm.dbg.declare(metadata %swift.type** %{{.*}}, metadata ![[TYPEARG:.*]], metadata !{{[0-9]+}}),
// CHECK: ![[TYPEARG]] = !DILocalVariable(name: "$swift.type.T"
// CHECK-SAME:                            type: ![[SWIFTMETATYPE:[^,)]+]]
// CHECK-SAME:                            flags: DIFlagArtificial
// CHECK: ![[SWIFTMETATYPE]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$swift.type",
// CHECK-SAME:                                baseType: ![[VOIDPTR:[0-9]+]]
// CHECK: ![[VOIDPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type, name: "_T0BpD", baseType: null
func aFunction<T : AProtocol>(_ x: T) {
    print("I am in aFunction: \(x.f())")
}

aFunction(AClass())

// Verify that we also emit a swift.type for a generic self.
class Foo<Bar> {
      func one() {
      }

      func two<Baz>(_ x: Baz) {
    // TODO: leave breadcrumbs for how to dynamically derive T in the debugger
    // CHECK- FIXME: !DILocalVariable(name: "$swift.type.Bar"
    // CHECK: !DILocalVariable(name: "$swift.type.Baz"
      }
}

// Verify that the backend doesn't elide the debug intrinsics.
// RUN: %target-swift-frontend %s -c -g -o %t.o
// RUN: %llvm-dwarfdump %t.o | %FileCheck %s --check-prefix=CHECK-LLVM
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "x"
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.T"
// CHECK- FIXME -LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.Bar"
// CHECK-LLVM-DAG:  .debug_str[{{.*}}] = "$swift.type.Baz"

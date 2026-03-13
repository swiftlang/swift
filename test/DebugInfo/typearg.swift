// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

protocol AProtocol {
  func f() -> String
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}

// CHECK: define hidden {{.*}}void @{{.*}}aFunction
// CHECK:  #dbg_declare(ptr %{{.*}}, ![[TYPEARG:.*]], !DIExpression(),
// CHECK-DAG: ![[TYPEARG]] = !DILocalVariable(name: "$\CF\84_0_0"{{.*}}type: ![[SWIFTMETATYPE:[^,)]+]]{{.*}}flags: DIFlagArtificial
// CHECK-DAG: ![[SWIFTMETATYPE]] = !DIDerivedType(tag: DW_TAG_typedef, name: "T",{{.*}}baseType: ![[VOIDPTR:[0-9]+]]
// CHECK-DAG: ![[VOIDPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type, name: "$sBpD", baseType: null
func aFunction<T : AProtocol>(_ x: T) {
    print("I am in aFunction: \(x.f())")
}

aFunction(AClass())

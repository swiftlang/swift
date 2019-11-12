// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - -module-name a \
// RUN:   -disable-debugger-shadow-copies | %FileCheck %s
public enum Result<Value> {
  case success(Value)
  case failure(Error)
}

extension Result {
  public func map<U>(_ transform: (Value) -> U) -> Result<U> {
    switch self {
    case .success(let value):
      return .success(transform(value))
    case .failure(let error):
      return .failure(error)
    }
  }
}

func use<T>(_ t : T) {
}

public class SomeClass {
  public let s = "hello"
}

extension Result where Value : SomeClass {
  public func f() -> Self {
    use(self)
    return map({ $0 })
  }  
}

extension Result {
  public func g() {
    use(self)
  }  
}

let x : Result<SomeClass> = .success(SomeClass())
let y : Result<(Int64, Int64, Int64, Int64)> = .success((1, 2, 3, 4))
x.f()
y.g()

// Here we have three types, all named $s1a6ResultOyxGD (---> a.Result<A>),
// but with different storage sizes:
//
//   0. Unsized from the subroutine tupe of map<U>.
//   1. Enum wrapping a pointer-sized object [map() and f()].
//   2. Enum wrapping a 4x64-bit tuple.
//
// Test that bound generic enums are *not* using their mangled name as a unique
// identifier.

// (0) unsized.
// CHECK: !DISubprogram(name: "map", {{.*}}line: 9, type: ![[SBTY:[0-9]+]]
// CHECK: ![[SBTY]] = !DISubroutineType(types: ![[SBTYS:[0-9]+]])
// CHECK: ![[SBTYS]] = !{!{{[0-9]+}}, !{{[0-9]+}}, ![[SELFTY:[0-9]+]]}
// CHECK: ![[SELFTY]] =
// CHECK-SAME:  !DICompositeType(tag: DW_TAG_structure_type, {{.*}}line: 3,
// CHECK-SAME:                   elements: ![[UNSIZED_ELTS:[0-9]+]]
// CHECK: ![[UNSIZED_ELTS]] = !{![[UNSIZED_MEM:[0-9]+]]}
// CHECK: ![[UNSIZED_MEM]] = !DIDerivedType(tag: DW_TAG_member,
// CHECK-SAME:                              baseType: ![[UNIQ:[0-9]+]]

// The unique unsized type.
// CHECK: ![[UNIQ]] = !DICompositeType(
// CHECK-SAME:            tag: DW_TAG_structure_type, name: "Result",
// CHECK-SAME:            line: 3,
// CHECK-NOT:             size:
// CHECK-SAME:            runtimeLang: DW_LANG_Swift,
// CHECK-SAME:            identifier: "$s1a6ResultOyxGD")

// (2)
// CHECK: !DILocalVariable(name: "self", arg: 2, {{.*}}line: 9,
// CHECK-SAME:             type: ![[C_TUP:[0-9]+]]
// CHECK: ![[C_TUP]] = !DIDerivedType(tag: DW_TAG_const_type,
// CHECK-SAME:                        baseType: ![[TUP:[0-9]+]])
// CHECK: ![[TUP]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                        line: 3, size: {{256|512}},

// (1)
// CHECK: !DILocalVariable(name: "self", arg: 1, {{.*}}line: 27,
// CHECK-SAME:             type: ![[C_CLASS:[0-9]+]]
// CHECK: ![[C_CLASS]] = !DIDerivedType(tag: DW_TAG_const_type,
// CHECK-SAME:                          baseType: ![[CLASS:[0-9]+]])
// CHECK: ![[CLASS]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                          line: 3, size:

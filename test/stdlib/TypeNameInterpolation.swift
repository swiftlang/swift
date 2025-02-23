// RUN: %target-build-swift -O %s -module-name=test -Xfrontend -sil-verify-all -emit-sil | %FileCheck %s
// RUN: %target-build-swift -Onone %s -module-name=test -Xfrontend -sil-verify-all -emit-sil | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -module-name=test %s -o %t/O.out
// RUN: %target-codesign %t/O.out
// RUN: %target-run %t/O.out
// RUN: %target-build-swift -Onone -module-name=test %s -o %t/Onone.out
// RUN: %target-codesign %t/Onone.out
// RUN: %target-run %t/Onone.out

// REQUIRES: executable_test

// Freestanding/minimal runtime does not support printing type names at runtime.
// UNSUPPORTED: freestanding

import StdlibUnittest

let TypeNameTests = TestSuite("TypeName")

class C {}
struct S {}
enum E {}

// Test unicode type names.
struct 🙂 { }
struct 🙃 { }

protocol P {}
protocol P2 {}
protocol P3 {}
protocol AssociatedTypes {
  associatedtype A
  associatedtype B
  associatedtype C
}

class Model : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

struct Model2 : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

class GC<T : AssociatedTypes> {}
struct GS<T : AssociatedTypes> {}
enum GE<T : AssociatedTypes> {}
class GC2<T : AssociatedTypes, U : AssociatedTypes> {}

class SomeOuterClass {
  struct SomeInnerStruct {}
  struct SomeInnerGenericStruct<T> {}
}

class SomeOuterGenericClass<T> {
  struct SomeInnerStruct {}
  struct SomeInnerGenericStruct<U> {}
}

extension SomeOuterGenericClass {
  struct OtherInnerStruct {}
  struct OtherInnerGenericStruct<U> {}
}

@inline(never)
func printTypename(_ s: String) {
  print(s)
}

// CHECK-LABEL: sil {{.*}} @$s4test0A21TypenameInterpolationyyF :
// CHECK-NOT:     $ss26DefaultStringInterpolationV06appendC0yyxlF
// CHECK-NOT:     _print_unlocked
// CHECK-NOT:     _typeName
// CHECK:       } // end sil function '$s4test0A21TypenameInterpolationyyF'
@inline(never)
func testTypenameInterpolation() {
  expectEqual("Int", "\(Int.self)")
  expectEqual("C", "\(C.self)")
  expectEqual("S", "\(S.self)")
  expectEqual("E", "\(E.self)")
  expectEqual("GC<Model>",
    "\(GC<Model>.self)")
  expectEqual("GS<Model>",
    "\(GS<Model>.self)")
  expectEqual("GE<Model>",
    "\(GE<Model>.self)")
  expectEqual("GC2<Model, Model2>",
    "\(GC2<Model, Model2>.self)")

  expectEqual("P", "\(P.self)")
  typealias PP2 = P & P2
  expectEqual("P & P2",
    "\(PP2.self)")
  expectEqual("Any", "\(Any.self)")
  expectEqual("P & P2", "\((P & P2).self)")

  expectEqual("🙂+🙃", "\(🙂.self)+\(🙃.self)")


  expectEqual("SomeInnerStruct",
              "\(SomeOuterClass.SomeInnerStruct.self)")
  expectEqual("SomeInnerGenericStruct<Int>",
              "\(SomeOuterClass.SomeInnerGenericStruct<Int>.self)")
  expectEqual("SomeInnerStruct",
              "\(SomeOuterGenericClass<Int>.SomeInnerStruct.self)")
  expectEqual("SomeInnerGenericStruct<Int>",
              "\(SomeOuterGenericClass<String>.SomeInnerGenericStruct<Int>.self)")
  expectEqual("OtherInnerStruct",
              "\(SomeOuterGenericClass<Int>.OtherInnerStruct.self)")
  expectEqual("OtherInnerGenericStruct<String>",
              "\(SomeOuterGenericClass<Int>.OtherInnerGenericStruct<String>.self)")
}

TypeNameTests.test("Interpolation") {
  testTypenameInterpolation()
}

runAllTests()

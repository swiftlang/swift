// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -O -emit-ir -primary-file %s | %FileCheck %s --check-prefix=CHECK-OPT -DINT=i%target-ptrsize

func use_metadata<F>(_ f: F) {}

func voidToVoid() {}
func intToInt(_ x: Int) -> Int { return x }

func cond() -> Bool { return true }

// CHECK: define hidden swiftcc void @"$S18metadata_dominance5test1yyF"()
func test1() {
// CHECK: call swiftcc i1 @"$S18metadata_dominance4condSbyF"()
  if cond() {
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$SyycMa"([[INT]] 0)
// CHECK: [[T0:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    use_metadata(voidToVoid)
// CHECK: call swiftcc i1 @"$S18metadata_dominance4condSbyF"()
// CHECK-NOT: @"$SyycMa"
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK-NOT: @"$SyycMa"
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T0]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$SyycMa"([[INT]] 0)
// CHECK: [[T1:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T1]])
  use_metadata(voidToVoid)
}

// CHECK: define hidden swiftcc void @"$S18metadata_dominance5test2yyF"()
func test2() {
// CHECK: call swiftcc i1 @"$S18metadata_dominance4condSbyF"()
  if cond() {
// CHECK: call swiftcc i1 @"$S18metadata_dominance4condSbyF"()
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$SyycMa"([[INT]] 0)
// CHECK: [[T0:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$SyycMa"([[INT]] 0)
// CHECK: [[T1:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T1]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$SyycMa"([[INT]] 0)
// CHECK: [[T2:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
// CHECK: call swiftcc void @"$S18metadata_dominance04use_A0yyxlF"(%swift.opaque* {{.*}}, %swift.type* [[T2]])
  use_metadata(voidToVoid)
}

protocol P {
    func makeFoo() -> Foo
}

class Foo: P {
    func makeFoo() -> Foo {
        fatalError()
    }
}

class SubFoo: Foo {
    final override func makeFoo() -> Foo {
        // Check that it creates an instance of type Foo,
        // and not an instance of a Self type involved
        // in this protocol conformance.
        return Foo()
    }
}

@inline(never)
func testMakeFoo(_ p: P) -> Foo.Type {
  let foo = p.makeFoo()
  return type(of: foo)
}

// The protocol witness for metadata_dominance.P.makeFoo () -> metadata_dominance.Foo in
// conformance metadata_dominance.Foo : metadata_dominance.P should not use the Self type
// as the type of the object to be created. It should dynamically obtain the type.
// CHECK-OPT-LABEL: define internal swiftcc %T18metadata_dominance3FooC* @"$S18metadata_dominance3FooCAA1PA2aDP04makeC0ACyFTW"
// CHECK-OPT-NOT: tail call noalias %swift.refcounted* @swift_allocObject(%swift.type* %Self


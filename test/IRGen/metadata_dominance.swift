// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -O -emit-ir -primary-file %s | %FileCheck %s --check-prefix=CHECK-OPT

func use_metadata<F>(_ f: F) {}

func voidToVoid() {}
func intToInt(_ x: Int) -> Int { return x }

func cond() -> Bool { return true }

// CHECK: define hidden swiftcc void @_TF18metadata_dominance5test1FT_T_()
func test1() {
// CHECK: call swiftcc i1 @_TF18metadata_dominance4condFT_Sb()
  if cond() {
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    use_metadata(voidToVoid)
// CHECK: call swiftcc i1 @_TF18metadata_dominance4condFT_Sb()
// CHECK-NOT: @_TMaFT_T_
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK-NOT: @_TMaFT_T_
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[T1:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T1]])
  use_metadata(voidToVoid)
}

// CHECK: define hidden swiftcc void @_TF18metadata_dominance5test2FT_T_()
func test2() {
// CHECK: call swiftcc i1 @_TF18metadata_dominance4condFT_Sb()
  if cond() {
// CHECK: call swiftcc i1 @_TF18metadata_dominance4condFT_Sb()
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK: [[T1:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T1]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[T2:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call swiftcc void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T2]])
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
// CHECK-OPT-LABEL: define hidden swiftcc %C18metadata_dominance3Foo* @_TTWC18metadata_dominance3FooS_1PS_FS1_7makeFoofT_S0_
// CHECK-OPT-NOT: tail call noalias %swift.refcounted* @swift_rt_swift_allocObject(%swift.type* %Self


// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil %s | %target-sil-opt -assume-parsing-unqualified-ownership-sil -enable-sil-verify-all -module-name="SILDeclRef"  - | %FileCheck %s

// Check that all SILDeclRefs are represented in the text form with a signature.
// This allows to avoid ambiguities which sometimes arise e.g. when a
// vtable or witness table contain multiple entries for a function with the same
// name but different signatures (like "foo" in the examples below).
//
// Check that SILParser can parse the new SIL syntax for SILDeclRefs by first
// producing the new syntax and then parsing it using sil-opt.

public protocol P {
  func foo() -> Int32
  func foo(n: Int32)
}

extension P {
  func boo() -> Int32 {
    return 1
  }
}

public class Base : P {
  public func foo() -> Int32 { return 2 }
  public func foo(n: Int32) {}
  public func foo(f: Float) -> Int32 { return 3 }
}

public class Derived1: Base {
  override public func foo() -> Int32 { return 4 }
  override public func foo(n: Int32) {}
  override public func foo(f: Float) -> Int32 { return 5 }
}

public class Derived2: Base {
  override public func foo() -> Int32 { return 6 }
  override public func foo(n: Int32) {}
  override public func foo(f: Float) -> Int32 { return 7 }
}

// CHECK-LABEL: sil @_TF10SILDeclRef5testPFT1pPS_1P__Vs5Int32
// Check that witness_method contains SILDeclRefs with a signature.
// CHECK: witness_method $@opened({{.*}}) P, #P.foo!1 : <Self where Self : P> (Self) -> () -> Int32, %{{.*}} : $*@opened({{.*}}) P : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> Int32
public func testP(p: P) -> Int32 {
  return p.foo()
}

// Check that class_method contains SILDeclRefs with a signature.
// CHECK-LABEL:sil @_TF10SILDeclRef8testBaseFT1bCS_4Base_Vs5Int32
// CHECK: class_method %{{.*}} : $Base, #Base.foo!1 : (Base) -> (Float) -> Int32, $@convention(method) (Float, @guaranteed Base) -> Int32
public func testBase(b: Base) -> Int32 {
  return b.foo(f: 10)
}

// Check that vtables and witness tables contain SILDeclRefs with signatures.

// CHECK: sil_vtable Base {
// CHECK:  #Base.foo!1: (Base) -> () -> Int32 : _TFC10SILDeclRef4Base3foofT_Vs5Int32	// Base.foo() -> Int32
// CHECK:  #Base.foo!1: (Base) -> (Int32) -> () : _TFC10SILDeclRef4Base3foofT1nVs5Int32_T_	// Base.foo(n : Int32) -> ()
// CHECK:  #Base.foo!1: (Base) -> (Float) -> Int32 : _TFC10SILDeclRef4Base3foofT1fSf_Vs5Int32	// Base.foo(f : Float) -> Int32
// CHECK:  #Base.deinit!deallocator: _TFC10SILDeclRef4BaseD	// Base.__deallocating_deinit
// CHECK:  #Base.init!initializer.1: (Base.Type) -> () -> Base : _TFC10SILDeclRef4BasecfT_S0_	// Base.init() -> Base
// CHECK: }

// CHECK:sil_witness_table [fragile] Base: P module SILDeclRef {
// CHECK: method #P.foo!1: <Self where Self : P> (Self) -> () -> Int32 : @_TTWC10SILDeclRef4BaseS_1PS_FS1_3foofT_Vs5Int32	// protocol witness for P.foo() -> Int32 in conformance Base
// CHECK: method #P.foo!1: <Self where Self : P> (Self) -> (Int32) -> () : @_TTWC10SILDeclRef4BaseS_1PS_FS1_3foofT1nVs5Int32_T_	// protocol witness for P.foo(n : Int32) -> () in conformance Base
// CHECK: }



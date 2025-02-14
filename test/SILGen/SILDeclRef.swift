// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s | %target-sil-opt -sil-print-types -parse-serialized-sil -enable-sil-verify-all -module-name="SILDeclRef"  - | %FileCheck %s

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

// CHECK-LABEL: sil @$s10SILDeclRef5testP1ps5Int32VAA1P_p_tF
// Check that witness_method contains SILDeclRefs with a signature.
// CHECK: witness_method $@opened({{.*}}, any P) Self, #P.foo : <Self where Self : P> (Self) -> () -> Int32, %{{.*}} : $*@opened({{.*}}, any P) Self : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> Int32
public func testP(p: P) -> Int32 {
  return p.foo()
}

// Check that class_method contains SILDeclRefs with a signature.
// CHECK-LABEL:sil @$s10SILDeclRef8testBase1bs5Int32VAA0D0C_tF
// CHECK: class_method %{{.*}} : $Base, #Base.foo : (Base) -> (Float) -> Int32, $@convention(method) (Float, @guaranteed Base) -> Int32
public func testBase(b: Base) -> Int32 {
  return b.foo(f: 10)
}

// Check that vtables and witness tables contain SILDeclRefs with signatures.

// CHECK: sil_vtable Base {
// CHECK-NEXT:  #Base.foo: (Base) -> () -> Int32 : @$s10SILDeclRef4BaseC3foos5Int32VyF	// Base.foo()
// CHECK-NEXT:  #Base.foo: (Base) -> (Int32) -> () : @$s10SILDeclRef4BaseC3foo1nys5Int32V_tF	// Base.foo(n:)
// CHECK-NEXT:  #Base.foo: (Base) -> (Float) -> Int32 : @$s10SILDeclRef4BaseC3foo1fs5Int32VSf_tF	// Base.foo(f:)
// CHECK-NEXT:  #Base.init!allocator: (Base.Type) -> () -> Base : @$s10SILDeclRef4BaseCACycfC
// CHECK-NEXT:  #Base.deinit!deallocator: @$s10SILDeclRef4BaseCfD	// Base.__deallocating_deinit
// CHECK-NEXT: }

// CHECK:sil_witness_table Base: P module SILDeclRef {
// CHECK-NEXT: method #P.foo: <Self where Self : P> (Self) -> () -> Int32 : @$s10SILDeclRef4BaseCAA1PA2aDP3foos5Int32VyFTW	// protocol witness for P.foo()
// CHECK-NEXT: method #P.foo: <Self where Self : P> (Self) -> (Int32) -> () : @$s10SILDeclRef4BaseCAA1PA2aDP3foo1nys5Int32V_tFTW	// protocol witness for P.foo(n:) in conformance Base
// CHECK-NEXT: }



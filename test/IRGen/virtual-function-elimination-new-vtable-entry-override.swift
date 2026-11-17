// RUN: %target-swift-frontend -enable-llvm-vfe -parse-as-library -module-name main -emit-ir %s | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

// A public override of an internal method gets a vtable slot of its own,
// because clients outside the module can't see the internal method's slot.
// For the classes below:
//
//   sil_vtable Derived {
//     #Base.f: ... : vtable thunk for Base.f() dispatching to Derived.f()
//     #Derived.f: ... : Derived.f()  // new slot introduced by the override
//   }
//
// Under VFE, that new slot uses Derived.f's own type ID.

public class Base {
  func f() {}  // internal
}

public class Derived: Base {
  // The dispatch thunk loads its slot with Derived.f's own type ID.
  // CHECK: @llvm.type.checked.load({{.*}}, metadata !"$s4main7DerivedC1fyyFTq")
  // The class metadata labels the new slot with that same type ID.
  // CHECK: !{i64 {{[0-9]+}}, !"$s4main7DerivedC1fyyFTq"}
  override public func f() {}
}

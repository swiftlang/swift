// RUN: %target-swift-frontend -enable-llvm-vfe -parse-as-library -module-name main -emit-ir %s | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

// An override that requires a new vtable entry uses its own type identifier.

public class Base {
  func f() {}
}

public class Derived: Base {
  // The dispatch thunk loads its slot with Derived.f's own type ID.
  // CHECK: @llvm.type.checked.load({{.*}}, metadata !"$s4main7DerivedC1fyyFTq")
  // The class metadata labels the new slot with that same type ID.
  // CHECK: !{i64 {{[0-9]+}}, !"$s4main7DerivedC1fyyFTq"}
  override public func f() {}
}

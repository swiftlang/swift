// RUN: %swift-frontend -primary-file %s %S/Inputs/noncopyable_private_other.swift -emit-ir -o - | %FileCheck %s

public struct SomeStruct : ~Copyable {
  // PublicType is a struct without a deinit but a non-copyable field with a
  // deinit of private type. So we can't access the private type's metadata.
  // Instead we have to call destroy on PublicType's metadata.
  public let x = PublicType()
}

// Call the destroy on the `PublicType` metadata instead of its private typed
// subfield.
// CHECK: define{{.*}} internal void @"$s19noncopyable_private10SomeStructVwxx"(
// CHECK: entry
// CHECK-NOT: define
// CHECK:  call void %Destroy(ptr {{.*}}, ptr @"$s19noncopyable_private10PublicTypeVN"
// CHECK:  ret void
// CHECK: }

// Similar issue with enums.

public struct SomeStructWithSPE : ~Copyable {
    public let x = PublicSinglePayloadEnum.empty
}

// CHECK: define{{.*}} internal void @"$s19noncopyable_private17SomeStructWithSPEVwxx"(
// CHECK: entry:
// CHECK-NOT: define
// CHECK:  call void %Destroy(ptr {{.*}}, ptr @"$s19noncopyable_private23PublicSinglePayloadEnumON"
// CHECK:  ret void
// CHECK: }

public struct SomeStructWithMPE : ~Copyable {
    public let x = PublicMultiPayloadEnum.empty
}


// CHECK: define{{.*}} internal void @"$s19noncopyable_private17SomeStructWithMPEVwxx"(
// CHECK: entry:
// CHECK-NOT: define
// CHECK:  call void %Destroy(ptr {{.*}}, ptr @"$s19noncopyable_private22PublicMultiPayloadEnumON"
// CHECK:  ret void
// CHECK: }

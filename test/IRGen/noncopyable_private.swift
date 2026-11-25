// RUN: %target-swift-frontend -primary-file %s %S/Inputs/noncopyable_private_other.swift -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend -O -primary-file %s %S/Inputs/noncopyable_private_other.swift -emit-ir -o - | %FileCheck %s --check-prefix=OPT --implicit-check-not='$s19noncopyable_private11PrivateType'

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

// The optimizer must not destroy PublicType's private field directly either.

public final class PublicTypeHolder {
  var p = PublicType()
}

@inline(never)
public func consumePublicType(_ p: consuming PublicType) {}

// OPT-LABEL: define {{.*}} @"$s19noncopyable_private16PublicTypeHolderCfD"(
// OPT:   call void %Destroy(ptr {{.*}}, ptr {{.*}}@"$s19noncopyable_private10PublicTypeVN")
// OPT:   ret void

// OPT-LABEL: define {{.*}} @"$s19noncopyable_private17consumePublicTypeyyAA0dE0VnF"(
// OPT:   call void %Destroy(ptr {{.*}}, ptr {{.*}}@"$s19noncopyable_private10PublicTypeVN")
// OPT:   ret void

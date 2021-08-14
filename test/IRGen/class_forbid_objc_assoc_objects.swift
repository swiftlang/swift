// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: @_METACLASS_DATA__TtC31class_forbid_objc_assoc_objects24AllowedToHaveAssocObject = internal constant { {{.*}} } { i32 129,
// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects24AllowedToHaveAssocObject = internal constant { {{.*}} } { i32 128,
final class AllowedToHaveAssocObject {
}

// CHECK: @_METACLASS_DATA__TtC31class_forbid_objc_assoc_objects24UnableToHaveAssocObjects = internal constant { {{.*}} } { i32 1153,
// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects24UnableToHaveAssocObjects = internal constant { {{.*}} } { i32 1152,
@_semantics("objc.forbidAssociatedObjects")
final class UnableToHaveAssocObjects {
}

// Class Metadata For Generic Metadata
//
// CHECK: [[CLASS_METADATA:@[0-9][0-9]*]] = internal constant <{ {{.*}} }> <{ {{.*}} { i32 1152,
//
// Generic Metadata Pattern
//
// CHECK: @"$s31class_forbid_objc_assoc_objects31UnableToHaveAssocObjectsGenericCMP" = internal constant {{.*}}[[CLASS_METADATA]]
@_semantics("objc.forbidAssociatedObjects")
final class UnableToHaveAssocObjectsGeneric<T> {
    var state: T
    init(state: T) { self.state = state }
}

// This should be normal.
//
// CHECK: @_METACLASS_DATA__TtC31class_forbid_objc_assoc_objects40UnsoundAbleToHaveAssocObjectsParentClass = internal constant { {{.*}} } { i32 129,
// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects40UnsoundAbleToHaveAssocObjectsParentClass = internal constant { {{.*}} } { i32 128,
class UnsoundAbleToHaveAssocObjectsParentClass {
}

// This should have assoc object constraints
//
// CHECK: @_METACLASS_DATA__TtC31class_forbid_objc_assoc_objects39UnsoundUnableToHaveAssocObjectsSubClass = internal constant { {{.*}} } { i32 1153,
// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects39UnsoundUnableToHaveAssocObjectsSubClass = internal constant { {{.*}} } { i32 1152,
@_semantics("objc.forbidAssociatedObjects")
final class UnsoundUnableToHaveAssocObjectsSubClass : UnsoundAbleToHaveAssocObjectsParentClass {
}

// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects41UnsoundAbleToHaveAssocObjectsParentClass2 = internal constant { {{.*}} } { i32 1152,
@_semantics("objc.forbidAssociatedObjects")
class UnsoundAbleToHaveAssocObjectsParentClass2 {
}

// This has normal metadata. We must at runtime add the flags of the subclass to
// the child.
//
// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects40UnsoundUnableToHaveAssocObjectsSubClass2 = internal constant { {{.*}} } { i32 128, 
final class UnsoundUnableToHaveAssocObjectsSubClass2 : UnsoundAbleToHaveAssocObjectsParentClass2 {
}

// CHECK: @_DATA__TtC31class_forbid_objc_assoc_objects40UnsoundUnableToHaveAssocObjectsSubClass3 = internal constant { {{.*}} } { i32 128,
class UnsoundUnableToHaveAssocObjectsSubClass3 : UnsoundAbleToHaveAssocObjectsParentClass2 {
}

class GenericAbleToHaveAssocObjectsParentClass<T> {
  public var state: T
  init(state: T) { self.state = state }
}

@_semantics("objc.forbidAssociatedObjects")
final class GenericUnableToHaveAssocObjectsSubClass<T> : GenericAbleToHaveAssocObjectsParentClass<T> {
}

@_semantics("objc.forbidAssociatedObjects")
class GenericAbleToHaveAssocObjectsParentClass2<T> {
  public var state: T
  init(state: T) { self.state = state }
}

final class GenericUnableToHaveAssocObjectsSubClass2<T> : GenericAbleToHaveAssocObjectsParentClass2<T> {
}

class GenericUnableToHaveAssocObjectsSubClass3<T> : GenericAbleToHaveAssocObjectsParentClass2<T> {
}

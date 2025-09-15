// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o -

// REQUIRES: OS=macosx
//
@_originallyDefinedIn(
     module: "Other", iOS 2.0, macOS 2.0, tvOS 2.0, watchOS 2.0)
@available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
public struct A {
    let i = 10
}

@_originallyDefinedIn(
     module: "Other", iOS 2.0, macOS 2.0, tvOS 2.0, watchOS 2.0)
@available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
public struct B {
    let i = 10
}

// Test that a type with an invalid @_originallyDefinedIn does not change the mangled name.
@_originallyDefinedIn(
     module: "Other", iOS 2.0, macOS 2.0, tvOS 2.0, watchOS 2.0)
@available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
private struct Invalid {
    let i = 20
}

// CHECK: ![[MOD:[0-9]+]] = !DIModule(scope: null, name: "originally_defined_in"
//
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "A",{{.*}}scope: ![[S:[0-9]+]]{{.*}}identifier: "$s5Other1AVD"
// CHECK: [[S]] = !DIModule({{.*}}name: "Other"

// CHECK: DICompositeType(tag: DW_TAG_structure_type, name: "Invalid",{{.*}}identifier: "$s21originally_defined_in

// CHECK: !DIImportedEntity(tag: DW_TAG_imported_declaration, name: "$s5Other1AVD",{{.*}}scope: ![[MOD]]
//
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s5Other1AV_ACtD",

let a = A()
let b = B.self
let c = (A(), A())
private let i = Invalid()

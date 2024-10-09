// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s


@_originallyDefinedIn(
     module: "Other", iOS 2.0, macOS 2.0, tvOS 2.0, watchOS 2.0)
@available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
public struct A {
    let i = 10
}

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "A", scope: ![[S1:[0-9]+]]
// CHECK-NEXT: [[S1]] = !DIModule({{.*}}name: "Other"

func f() {
    let a = A()
}

f()

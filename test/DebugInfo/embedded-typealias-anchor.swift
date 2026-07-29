// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g -enable-experimental-feature Embedded -wmo -disable-availability-checking -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// A type alias (Handler) that only ever appears as a generic argument of a
// stored property's type (handler: Handler?). The enclosing bound-generic type
// is emitted by mangled name only, without recursing into its generic
// arguments, so the alias must still be anchored with its own DW_TAG_typedef.

// CHECK-DAG: ![[TYPEDEF:[0-9]+]] = !DIDerivedType(tag: DW_TAG_typedef, name: "{{.*}}7StorageC7Handlera{{.*}}", scope: {{.*}}baseType: ![[CLOSURE:[0-9]+]]
// CHECK-DAG: ![[CLOSURE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$eyxYbcD",{{.*}}identifier: "$eyxYbcD")

final class Storage<Element> {
  typealias Handler = @Sendable (Element) -> Void
  var handler: Handler? = nil
}

let g: Storage<Int>? = nil

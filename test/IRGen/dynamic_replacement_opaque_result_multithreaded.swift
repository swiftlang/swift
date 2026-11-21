// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -module-name A -wmo -num-threads 2 -enable-anonymous-context-mangled-names -emit-ir %t/a.swift %t/b.swift -o %t/a.ll -o %t/b.ll
// RUN: %FileCheck %s < %t/a.ll
// RUN: %FileCheck %s --check-prefix=B < %t/b.ll

// UNSUPPORTED: CPU=wasm32

// The replacement tables in the primary module directly reference the
// replacements' opaque type descriptors, so those must be defined there too
// (rdar://187571058). Other modules reference them indirectly. S (internal)
// and T (public) cover the lazily and eagerly emitted descriptors.

// CHECK-DAG: @"$s1A1SV1yQrvpQOMQ" = {{.*}}constant
// CHECK-DAG: @"$s1A1TV1yQrvpQOMQ" = {{.*}}constant
// CHECK-DAG: @"\01l_auto_dynamic_replacements_some" = {{.*}} @"$s1A1SV1yQrvpQOMQ" to {{.*}} @"$s1A1TV1yQrvpQOMQ" to
// B: @"got.$s1A1SV1yQrvpQOMQ"

//--- a.swift
struct S {
  dynamic var x: some Equatable { 1 }
}
public struct T {
  public dynamic var x: some Equatable { 1 }
}

//--- b.swift
extension S {
  @_dynamicReplacement(for: x)
  var y: some Equatable { 2 }
}
extension T {
  @_dynamicReplacement(for: x)
  public var y: some Equatable { 2 }
}

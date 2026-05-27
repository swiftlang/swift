// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Tests that tuples with no known size are not given a size in DWARF.

protocol Prot {
  associatedtype Associated
}

func generic_func<B: Prot>(_ b: B) {
  let x: (first: Int, optionalSecond: B.Associated?) = (first: 0, optionalSecond: nil)
  _ = x
}

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, {{.*}}name: "$sSi5first_10Associated18generic_tuple_size4ProtPQzSg14optionalSecondtD"
// CHECK-NOT: size:
// CHECK-SAME:)
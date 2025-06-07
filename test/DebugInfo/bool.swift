// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:   | %FileCheck %s --check-prefix=CHECK_G

func markUsed<T>(_ t: T) {}

// Int1 uses 1 bit, but is aligned at 8 bits.
// CHECK: !DIBasicType(name: "$sBi1_D", size: 1, encoding: DW_ATE_unsigned, num_extra_inhabitants: 254)
// Bool has a fixed layout with a storage size of 1 byte and 7 "spare" bits.
// CHECK_G: !DICompositeType(tag: DW_TAG_structure_type, name: "Bool",
// CHECK_G-SAME:             size: 8
func main() {
  var t = true
  var f = false
  markUsed("hello")
}

main()

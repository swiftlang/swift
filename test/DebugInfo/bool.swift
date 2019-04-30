// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:   | %FileCheck %s --check-prefix=CHECK_G

func markUsed<T>(_ t: T) {}

// Bool has a fixed layout with a storage size of 1 byte.
// CHECK_G: !DICompositeType(tag: DW_TAG_structure_type, name: "Bool",
// CHECK_G-SAME:             size: 8
func main() {
  var t = true
  var f = false
  markUsed("hello")
}

main()

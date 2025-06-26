// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)


import VirtualMethodsBasic

// func callVirtualRenamedMethod(_ b: Base) {
//   b.virtualRename()
// }

@available(SwiftStdlib 5.8, *)
func callsRenamedVirtualMethodsInFRT(_ i: Immortal, _ c: Child) {
  // i.virtualMethod()
  i.swiftVirtualRename()
  // c.swiftVirtualRename()
}



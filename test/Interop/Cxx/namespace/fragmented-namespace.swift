// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Namespace.swiftmodule -I %S/Inputs -enable-cxx-interop %s

import NamespaceDeclA

// Ensure  Swift correctly deals with fragmented namespaces
public func addOneAndTwo() -> CInt {
  return outer.nested.getOne() +  outer.nested.getTwo()
}

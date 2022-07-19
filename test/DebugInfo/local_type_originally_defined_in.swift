// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/local_type_originally_defined_in_other.swiftmodule %S/Inputs/local_type_originally_defined_in_other.swift
// RUN: %target-swift-frontend -I%t -g -emit-ir %s

import local_type_originally_defined_in_other

public func localTypeAliasTest(horse: Horse) {
  // The local type mangling for 'A' mentions 'Horse', which must
  // be mangled using it's current module name, and not the
  // original module name, for consistency with the debug info
  // mangling.
  typealias A = Int

  let info = UnsafeMutablePointer<A>.allocate(capacity: 1)
  _ = info
}
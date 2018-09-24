// RUN: %empty-directory(%t)
// RUN: echo 'client()' >%t/main.swift
// RUN: %target-swiftc_driver -module-name AttrImplFP -emit-module -emit-module-path %t/AttrImplFP.swiftmodule -emit-library -o %t/library.%target-dylib-extension %S/attr_implements_fp.swift
// RUN: %target-swiftc_driver -I %t -o %t/a.out %s %t/main.swift %t/library.%target-dylib-extension
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/library.%target-dylib-extension
// RUN: %target-run %t/a.out %t/library.%target-dylib-extension | %FileCheck %s
// REQUIRES: executable_test

// This test just checks that the lookup-table entries for @_implements are
// also written-to and read-from serialized .swiftmodules

import AttrImplFP

public func client() {
  assert(compare_Comparables(Fauxt.one, Fauxt.two))
  assert(comparedAsComparablesCount == 1)
  // CHECK: compared as Comparables
  assert(compare_Comparables(Fauxt.one, Fauxt.nan))
  assert(comparedAsComparablesCount == 2)
  // CHECK: compared as Comparables
  assert(!compare_Comparables(Fauxt.nan, Fauxt.one))
  assert(comparedAsComparablesCount == 3)
  // CHECK: compared as Comparables

  assert(compare_Fauxts(Fauxt.one, Fauxt.two))
  assert(comparedAsFauxtsCount == 1)
  // CHECK: compared as Fauxts
  assert(!compare_Fauxts(Fauxt.one, Fauxt.nan))
  assert(comparedAsFauxtsCount == 2)
  // CHECK: compared as Fauxts
  assert(!compare_Fauxts(Fauxt.nan, Fauxt.one))
  assert(comparedAsFauxtsCount == 3)
  // CHECK: compared as Fauxts
}

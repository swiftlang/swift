// RUN: %empty-directory(%t)
// RUN: echo 'client()' >%t/main.swift
// RUN: %target-build-swift-dylib(%t/libAttrImplFP.%target-dylib-extension) -module-name AttrImplFP -emit-module -emit-module-path %t/AttrImplFP.swiftmodule %S/attr_implements_fp.swift
// RUN: %target-build-swift -I %t -o %t/a.out %s %t/main.swift -L %t -Xlinker -rpath -Xlinker %t -lAttrImplFP
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/libAttrImplFP.%target-dylib-extension
// RUN: %target-run %t/a.out %t/libAttrImplFP.%target-dylib-extension | %FileCheck %s
// REQUIRES: executable_test

// This test just checks that the lookup-table entries for @_implements are
// also written-to and read-from serialized .swiftmodules

import AttrImplFP

public func client() {
  precondition(compare_Comparables(Fauxt.one, Fauxt.two))
  precondition(comparedAsComparablesCount == 1)
  // CHECK: compared as Comparables
  precondition(compare_Comparables(Fauxt.one, Fauxt.nan))
  precondition(comparedAsComparablesCount == 2)
  // CHECK: compared as Comparables
  precondition(!compare_Comparables(Fauxt.nan, Fauxt.one))
  precondition(comparedAsComparablesCount == 3)
  // CHECK: compared as Comparables

  precondition(compare_Fauxts(Fauxt.one, Fauxt.two))
  precondition(comparedAsFauxtsCount == 1)
  // CHECK: compared as Fauxts
  precondition(!compare_Fauxts(Fauxt.one, Fauxt.nan))
  precondition(comparedAsFauxtsCount == 2)
  // CHECK: compared as Fauxts
  precondition(!compare_Fauxts(Fauxt.nan, Fauxt.one))
  precondition(comparedAsFauxtsCount == 3)
  // CHECK: compared as Fauxts
}

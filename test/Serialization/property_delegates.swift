// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_property_delegates.swift
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown

// Same test, but with -enable-testing so we can see the backing properties
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_property_delegates.swift -enable-testing
// RUN: %target-swift-frontend -DTESTING -typecheck -I%t %s

#if TESTING
@testable import def_property_delegates
#else
import def_property_delegates
#endif

func useDelegates(hd: HasDelegates) {
  // Access the original properties
  let _: Int = hd.x

  let _: SomeDelegate<Int> = hd.$x // expected-error{{'$x' is inaccessible due to 'internal' protection level}}

  var mutableHD = hd
  mutableHD.x = 17

  mutableHD.$x = SomeDelegate(initialValue: 42) // expected-error{{'$x' is inaccessible due to 'internal' protection level}}
}

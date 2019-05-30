// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_property_wrappers.swift
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown

// Same test, but with -enable-testing so we can see the backing properties
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_property_wrappers.swift -enable-testing
// RUN: %target-swift-frontend -DTESTING -typecheck -I%t %s

#if TESTING
@testable import def_property_wrappers
#else
import def_property_wrappers
#endif

func useWrappers(hd: HasWrappers) {
  // Access the original properties
  let _: Int = hd.x

  let _: SomeWrapper<Int> = hd.$x // expected-error{{'$x' is inaccessible due to 'internal' protection level}}

  var mutableHD = hd
  mutableHD.x = 17

  mutableHD.$x = SomeWrapper(initialValue: 42) // expected-error{{'$x' is inaccessible due to 'internal' protection level}}
}

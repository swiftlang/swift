// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_property_delegates.swift
// RUN: %target-swift-frontend -typecheck -I%t -verify -verify-ignore-unknown %s

import def_property_delegates

func useDelegates(hd: HasDelegates) {
  // Access the original properties
  let _: Int = hd.x
  let _: Int = hd.y
  let _: Int = hd.z

  // Access the backing properties
  _ = hd.$x // expected-error{{'$x' is inaccessible due to 'internal' protection level}}
  _ = hd.$y // okay
  let _: Int = hd.$y // expected-error{{cannot convert value of type 'SomeDelegate<Int>' to specified type 'Int'}}
  _ = hd.$z // OK: 
  let _: Int = hd.$z // expected-error{{cannot convert value of type 'SomeDelegate<Int>' to specified type 'Int'}}

  var mutableHD = hd
  mutableHD.$z = hd.$z // expected-error{{cannot assign to property: '$z' is immutable}}
}

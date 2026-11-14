// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -disable-availability-checking -parse-as-library -plugin-path %swift-plugin-dir

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Observation

@Observable // expected-error {{'@Observable' cannot be applied to actor type 'MyActor'}}
actor MyActor {}

@Observable // expected-error {{'@Observable' cannot be applied to enumeration type 'MyEnum'}}
enum MyEnum {
  case myCase // required for macro invocation
}

@Observable // expected-error {{'@Observable' cannot be applied to struct type 'MyStruct'}}
struct MyStruct {}

// Members don't produce further errors from '@ObservationTracked'.

@Observable // expected-error {{'@Observable' cannot be applied to actor type 'MyActorWithProperty'}}
actor MyActorWithProperty {
  var value = 0
}

@Observable // expected-error {{'@Observable' cannot be applied to enumeration type 'MyEnumWithProperty'}}
enum MyEnumWithProperty {
  case myCase
  var value: Int { 0 }
}

@Observable // expected-error {{'@Observable' cannot be applied to struct type 'MyStructWithProperty'}}
struct MyStructWithProperty {
  var value = 0
}

@Observable // expected-error {{'@Observable' cannot be applied to struct type 'MyConformingStruct'}}
struct MyConformingStruct: Observable {
  var value = 0
}
